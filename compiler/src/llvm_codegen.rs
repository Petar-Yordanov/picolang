use std::collections::HashMap;
use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module as LlvmModule;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue,
};
use inkwell::{AddressSpace, OptimizationLevel};

use crate::ast::Type;
use crate::ir::{
    BinOp as IrBinOp, BlockId, Instr, InstrKind, IrFunction, IrModule, Terminator, UnOp as IrUnOp,
    ValueId,
};
use inkwell::values::AsValueRef;

pub fn compile_ir_to_object(
    ir: &IrModule,
    module_name: &str,
    out_path: &Path,
) -> Result<(), String> {
    let target_machine = get_host_target_machine().map_err(|e| e.to_string())?;

    let context = Context::create();
    let module = context.create_module(module_name);
    let builder = context.create_builder();

    {
        let mut cg = CodeGen::new(ir, &context, &module, &builder, &target_machine);
        cg.lower_module()?;
    }

    module
        .verify()
        .map_err(|e| format!("LLVM module verification failed: {}", e.to_string()))?;

    target_machine
        .write_to_file(&module, FileType::Object, out_path)
        .map_err(|e| format!("failed to write object file: {}", e.to_string()))
}

fn get_host_target_machine() -> Result<TargetMachine, Box<dyn std::error::Error>> {
    Target::initialize_native(&InitializationConfig::default())?;

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple)?;

    let cpu = TargetMachine::get_host_cpu_name();
    let features = TargetMachine::get_host_cpu_features();

    let opt_level = OptimizationLevel::Default;
    let reloc_mode = RelocMode::Default;
    let code_model = CodeModel::Default;

    target
        .create_target_machine(
            &triple,
            cpu.to_str()?,
            features.to_str()?,
            opt_level,
            reloc_mode,
            code_model,
        )
        .ok_or_else(|| "failed to create target machine".into())
}

#[allow(unused)]
struct CodeGen<'ctx, 'm> {
    ir: &'m IrModule,
    context: &'ctx Context,
    module: &'m LlvmModule<'ctx>,
    builder: &'m Builder<'ctx>,
    target_machine: &'m TargetMachine,

    // Map function name to an LLVM function value
    fn_map: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx, 'm> CodeGen<'ctx, 'm> {
    fn new(
        ir: &'m IrModule,
        context: &'ctx Context,
        module: &'m LlvmModule<'ctx>,
        builder: &'m Builder<'ctx>,
        target_machine: &'m TargetMachine,
    ) -> Self {
        Self {
            ir,
            context,
            module,
            builder,
            target_machine,
            fn_map: HashMap::new(),
        }
    }

    fn lower_module(&mut self) -> Result<(), String> {
        for func in &self.ir.functions {
            let llvm_fn = self.declare_function(func)?;
            self.fn_map.insert(func.name.clone(), llvm_fn);
        }

        for func in &self.ir.functions {
            self.define_function(func)?;
        }

        Ok(())
    }

    fn declare_function(&self, func: &IrFunction) -> Result<FunctionValue<'ctx>, String> {
        let param_tys: Vec<BasicMetadataTypeEnum<'ctx>> = func
            .params
            .iter()
            .map(|vid| {
                let instr = self
                    .find_instr(func, *vid)
                    .unwrap_or_else(|| panic!("param ValueId {:?} not found", vid));

                let bt = self.llvm_type(&instr.ty);
                bt.into()
            })
            .collect();

        let fn_ty = match func.ret_ty {
            Type::Void => self.context.void_type().fn_type(&param_tys, false),
            ref other => {
                let ret_bt = self.llvm_type(other);
                ret_bt.fn_type(&param_tys, false)
            }
        };

        let llvm_fn = self
            .module
            .get_function(&func.name)
            .unwrap_or_else(|| self.module.add_function(&func.name, fn_ty, None));

        Ok(llvm_fn)
    }

    fn define_function(&self, func: &IrFunction) -> Result<(), String> {
        let llvm_fn = *self
            .fn_map
            .get(&func.name)
            .unwrap_or_else(|| panic!("function {} not declared", func.name));

        let mut bb_map: HashMap<BlockId, inkwell::basic_block::BasicBlock<'ctx>> = HashMap::new();
        for block in &func.blocks {
            let bb = self
                .context
                .append_basic_block(llvm_fn, &format!("bb{}", block.id.0));
            bb_map.insert(block.id, bb);
        }

        let entry_bb = bb_map
            .get(&func.entry)
            .cloned()
            .unwrap_or_else(|| self.context.append_basic_block(llvm_fn, "entry"));
        let entry_builder = self.context.create_builder();
        entry_builder.position_at_end(entry_bb);

        let mut fcx = FunctionCtx {
            codegen: self,
            func,
            llvm_fn,
            bb_map,
            entry_bb,
            entry_builder,
            value_map: HashMap::new(),
            var_allocas: HashMap::new(),
        };

        for block in &func.blocks {
            let bb = *fcx
                .bb_map
                .get(&block.id)
                .expect("missing basic block in map");

            fcx.codegen.builder.position_at_end(bb);

            for instr in &block.instrs {
                let val = fcx.lower_instr(instr)?;
                if let Some(v) = val {
                    fcx.value_map.insert(instr.id, v);
                }
            }

            let term = block
                .term
                .as_ref()
                .unwrap_or_else(|| panic!("block {:?} missing terminator", block.id));
            fcx.lower_terminator(term)?;
        }

        for bb in llvm_fn.get_basic_blocks() {
            if bb.get_terminator().is_none() {
                self.builder.position_at_end(bb);

                match &func.ret_ty {
                    Type::Void => {
                        self.builder.build_return(None).map_err(|e| e.to_string())?;
                    }
                    other_ty => {
                        let bt = self.llvm_type(other_ty);

                        let zero = match bt {
                            BasicTypeEnum::IntType(t) => {
                                t.const_int(0, false).as_basic_value_enum()
                            }
                            BasicTypeEnum::PointerType(t) => t.const_null().as_basic_value_enum(),
                            BasicTypeEnum::FloatType(t) => t.const_float(0.0).as_basic_value_enum(),
                            BasicTypeEnum::ArrayType(t) => t.const_zero().as_basic_value_enum(),
                            BasicTypeEnum::StructType(t) => t.const_zero().as_basic_value_enum(),
                            BasicTypeEnum::VectorType(t) => t.const_zero().as_basic_value_enum(),
                            BasicTypeEnum::ScalableVectorType(_) => {
                                unreachable!(
                                    "scalable vectors are not used as return types in this backend"
                                )
                            }
                        };

                        self.builder
                            .build_return(Some(&zero))
                            .map_err(|e| e.to_string())?;
                    }
                }
            }
        }

        Ok(())
    }

    fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Int => self.context.i32_type().as_basic_type_enum(),
            Type::Bool => self.context.bool_type().as_basic_type_enum(),

            Type::Char | Type::Byte => self.context.i8_type().as_basic_type_enum(),

            Type::String | Type::Array(_) => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::from(0u16))
                .as_basic_type_enum(),

            Type::Named { .. } => self.context.i32_type().as_basic_type_enum(),

            Type::Void => self.context.i32_type().as_basic_type_enum(),
        }
    }

    fn find_instr<'a>(&self, func: &'a IrFunction, id: ValueId) -> Option<&'a Instr> {
        for block in &func.blocks {
            for instr in &block.instrs {
                if instr.id == id {
                    return Some(instr);
                }
            }
        }
        None
    }
}

struct FunctionCtx<'a, 'ctx, 'm> {
    codegen: &'a CodeGen<'ctx, 'm>,
    func: &'a IrFunction,
    llvm_fn: FunctionValue<'ctx>,

    bb_map: HashMap<BlockId, inkwell::basic_block::BasicBlock<'ctx>>,

    entry_bb: inkwell::basic_block::BasicBlock<'ctx>,
    entry_builder: Builder<'ctx>,

    value_map: HashMap<ValueId, BasicValueEnum<'ctx>>,

    var_allocas: HashMap<String, PointerValue<'ctx>>,
}

impl<'a, 'ctx, 'm> FunctionCtx<'a, 'ctx, 'm> {
    fn get_extern_print_int(&self) -> FunctionValue<'ctx> {
        let name = "printInt";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }

        let ctx = self.codegen.context;
        let i32_ty = ctx.i32_type();
        let fn_ty = ctx.void_type().fn_type(&[i32_ty.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_extern_print_char(&self) -> FunctionValue<'ctx> {
        let name = "printChar";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }

        let ctx = self.codegen.context;
        let i8_ty = ctx.i8_type();
        let fn_ty = ctx.void_type().fn_type(&[i8_ty.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_extern_print_string(&self) -> FunctionValue<'ctx> {
        let name = "printString";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }

        let ctx = self.codegen.context;
        let i8_ptr_ty = ctx.i8_type().ptr_type(AddressSpace::from(0u16));
        let fn_ty = ctx.void_type().fn_type(&[i8_ptr_ty.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    /// log.logInfo("…") in source needs to call C symbol 'log_logInfo' defined in the runtime
    fn get_extern_log_log_info(&self) -> FunctionValue<'ctx> {
        let name = "log_logInfo";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }

        let ctx = self.codegen.context;
        let i8_ptr_ty = ctx.i8_type().ptr_type(AddressSpace::from(0u16));
        let fn_ty = ctx.void_type().fn_type(&[i8_ptr_ty.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_extern_runtime_read_file(&self) -> FunctionValue<'ctx> {
        let name = "runtime_read_file";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }

        let ctx = self.codegen.context;
        let i8_ptr_ty = ctx.i8_type().ptr_type(AddressSpace::from(0u16));
        let fn_ty = i8_ptr_ty.fn_type(&[i8_ptr_ty.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn lower_instr(&mut self, instr: &Instr) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        use InstrKind::*;

        let result = match &instr.kind {
            ConstInt(i) => {
                let v = self
                    .codegen
                    .context
                    .i32_type()
                    .const_int(*i as u64, true)
                    .as_basic_value_enum();
                Some(v)
            }

            ConstBool(b) => {
                let v = self
                    .codegen
                    .context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false)
                    .as_basic_value_enum();
                Some(v)
            }

            ConstChar(ch) => {
                let v = self
                    .codegen
                    .context
                    .i8_type()
                    .const_int(*ch as u64, false)
                    .as_basic_value_enum();
                Some(v)
            }

            ConstString(s) => {
                let name = format!(".str_{}", instr.id.0);
                let gv = self
                    .codegen
                    .builder
                    .build_global_string_ptr(s, &name)
                    .map_err(|e| e.to_string())?;
                let ptr = gv.as_pointer_value();
                Some(ptr.as_basic_value_enum())
            }

            Param { index } => {
                let v = self
                    .llvm_fn
                    .get_nth_param(*index as u32)
                    .ok_or_else(|| format!("param index {} out of bounds", index))?;
                Some(v)
            }

            BinOp { op, lhs, rhs } => {
                let builder = self.codegen.builder;

                let val: IntValue<'ctx> = match op {
                    IrBinOp::And | IrBinOp::Or => {
                        let lhs_b = self.expect_bool(*lhs)?;
                        let rhs_b = self.expect_bool(*rhs)?;

                        match op {
                            IrBinOp::And => builder
                                .build_and(lhs_b, rhs_b, "andtmp")
                                .map_err(|e| e.to_string())?,
                            IrBinOp::Or => builder
                                .build_or(lhs_b, rhs_b, "ortmp")
                                .map_err(|e| e.to_string())?,
                            _ => unreachable!(),
                        }
                    }

                    _ => {
                        let lhs_v = self.expect_int(*lhs)?;
                        let rhs_v = self.expect_int(*rhs)?;
                        let (lhs_n, rhs_n) = self.coerce_ints(lhs_v, rhs_v)?;

                        match op {
                            IrBinOp::Add => builder
                                .build_int_add(lhs_n, rhs_n, "addtmp")
                                .map_err(|e| e.to_string())?,
                            IrBinOp::Sub => builder
                                .build_int_sub(lhs_n, rhs_n, "subtmp")
                                .map_err(|e| e.to_string())?,
                            IrBinOp::Mul => builder
                                .build_int_mul(lhs_n, rhs_n, "multmp")
                                .map_err(|e| e.to_string())?,
                            IrBinOp::Div => builder
                                .build_int_signed_div(lhs_n, rhs_n, "divtmp")
                                .map_err(|e| e.to_string())?,

                            IrBinOp::Eq => builder
                                .build_int_compare(
                                    inkwell::IntPredicate::EQ,
                                    lhs_n,
                                    rhs_n,
                                    "cmptmp",
                                )
                                .map_err(|e| e.to_string())?,
                            IrBinOp::NotEq => builder
                                .build_int_compare(
                                    inkwell::IntPredicate::NE,
                                    lhs_n,
                                    rhs_n,
                                    "cmptmp",
                                )
                                .map_err(|e| e.to_string())?,
                            IrBinOp::Lt => builder
                                .build_int_compare(
                                    inkwell::IntPredicate::SLT,
                                    lhs_n,
                                    rhs_n,
                                    "cmptmp",
                                )
                                .map_err(|e| e.to_string())?,
                            IrBinOp::LtEq => builder
                                .build_int_compare(
                                    inkwell::IntPredicate::SLE,
                                    lhs_n,
                                    rhs_n,
                                    "cmptmp",
                                )
                                .map_err(|e| e.to_string())?,
                            IrBinOp::Gt => builder
                                .build_int_compare(
                                    inkwell::IntPredicate::SGT,
                                    lhs_n,
                                    rhs_n,
                                    "cmptmp",
                                )
                                .map_err(|e| e.to_string())?,
                            IrBinOp::GtEq => builder
                                .build_int_compare(
                                    inkwell::IntPredicate::SGE,
                                    lhs_n,
                                    rhs_n,
                                    "cmptmp",
                                )
                                .map_err(|e| e.to_string())?,

                            IrBinOp::And | IrBinOp::Or => unreachable!(),
                        }
                    }
                };

                Some(val.as_basic_value_enum())
            }

            UnOp { op, value } => {
                let v = self.expect_int(*value)?;
                let builder = self.codegen.builder;
                let res: IntValue<'ctx> = match op {
                    IrUnOp::Neg => builder
                        .build_int_neg(v, "negtmp")
                        .map_err(|e| e.to_string())?,
                    IrUnOp::Not => builder.build_not(v, "nottmp").map_err(|e| e.to_string())?,
                };
                Some(res.as_basic_value_enum())
            }

            StoreVar { name, value } => {
                let src = if let Some(v) = self.value_map.get(value).cloned() {
                    v
                } else {
                    let bt = self.codegen.llvm_type(&instr.ty);
                    match bt {
                        BasicTypeEnum::IntType(t) => t.const_int(0, false).as_basic_value_enum(),
                        BasicTypeEnum::PointerType(t) => t.const_null().as_basic_value_enum(),
                        BasicTypeEnum::FloatType(t) => t.const_float(0.0).as_basic_value_enum(),
                        BasicTypeEnum::ArrayType(t) => t.const_zero().as_basic_value_enum(),
                        BasicTypeEnum::StructType(t) => t.const_zero().as_basic_value_enum(),
                        BasicTypeEnum::VectorType(t) => t.const_zero().as_basic_value_enum(),
                        BasicTypeEnum::ScalableVectorType(_) => {
                            unreachable!("scalable vectors not used in StoreVar")
                        }
                    }
                };

                let ty = src.get_type();
                let alloca = self.get_or_create_alloca(name, ty)?;

                self.codegen
                    .builder
                    .build_store(alloca, src)
                    .map_err(|e| e.to_string())?;
                None
            }

            LoadVar { name } => {
                if let Some(alloca) = self.var_allocas.get(name).cloned() {
                    let loaded = self
                        .codegen
                        .builder
                        .build_load(alloca, &format!("load_{}", name))
                        .map_err(|e| e.to_string())?;
                    Some(loaded)
                } else {
                    let bt = self.codegen.llvm_type(&instr.ty);

                    let zero = match bt {
                        BasicTypeEnum::IntType(t) => t.const_int(0, false).as_basic_value_enum(),
                        BasicTypeEnum::PointerType(t) => t.const_null().as_basic_value_enum(),
                        BasicTypeEnum::FloatType(t) => t.const_float(0.0).as_basic_value_enum(),
                        BasicTypeEnum::ArrayType(t) => t.const_zero().as_basic_value_enum(),
                        BasicTypeEnum::StructType(t) => t.const_zero().as_basic_value_enum(),
                        BasicTypeEnum::VectorType(t) => t.const_zero().as_basic_value_enum(),
                        BasicTypeEnum::ScalableVectorType(_) => {
                            unreachable!("scalable vectors not used in LoadVar fallback")
                        }
                    };

                    Some(zero)
                }
            }

            ArrayLit { elem_ty, elems } => match elem_ty {
                Type::Int => {
                    let i32_ty = self.codegen.context.i32_type();
                    let len_const = i32_ty.const_int(elems.len() as u64, false);

                    let new_fn = self.get_runtime_array_new_int_fn();
                    let call_site = self
                        .codegen
                        .builder
                        .build_call(new_fn, &[len_const.into()], "array_new_int")
                        .map_err(|e| e.to_string())?;

                    let arr_val = unsafe { BasicValueEnum::new(call_site.as_value_ref()) };
                    let arr_ptr = arr_val.into_pointer_value();

                    if !elems.is_empty() {
                        let set_fn = self.get_runtime_array_set_int_fn();

                        for (idx, elem_vid) in elems.iter().enumerate() {
                            let elem_val = self.expect_int(*elem_vid)?;
                            let idx_const = i32_ty.const_int(idx as u64, false);

                            self.codegen
                                .builder
                                .build_call(
                                    set_fn,
                                    &[arr_ptr.into(), idx_const.into(), elem_val.into()],
                                    "array_set_int",
                                )
                                .map_err(|e| e.to_string())?;
                        }
                    }

                    Some(arr_ptr.as_basic_value_enum())
                }
                other => {
                    return Err(format!(
                        "LLVM backend: ArrayLit for element type {:?} not supported yet",
                        other
                    ));
                }
            },

            Index { target, index } => {
                let arr_val = self
                    .value_map
                    .get(target)
                    .cloned()
                    .ok_or_else(|| format!("unknown value id {:?} in Index target", target))?;

                let arr_ptr = match arr_val {
                    BasicValueEnum::PointerValue(p) => p,
                    other => {
                        return Err(format!(
                            "Index expects pointer-valued array, got {:?}",
                            other
                        ));
                    }
                };

                let idx_val = self.expect_int(*index)?;

                let get_fn = self.get_runtime_array_get_int_fn();
                let call_site = self
                    .codegen
                    .builder
                    .build_call(get_fn, &[arr_ptr.into(), idx_val.into()], "array_get_int")
                    .map_err(|e| e.to_string())?;

                let elem_val =
                    unsafe { BasicValueEnum::new(call_site.as_value_ref()) }.into_int_value();

                Some(elem_val.as_basic_value_enum())
            }

            StructLit { typ, .. } => {
                let bt = self.codegen.llvm_type(typ);
                let zero = match bt {
                    BasicTypeEnum::IntType(t) => t.const_int(0, false).as_basic_value_enum(),
                    BasicTypeEnum::PointerType(t) => t.const_null().as_basic_value_enum(),
                    BasicTypeEnum::FloatType(t) => t.const_float(0.0).as_basic_value_enum(),
                    BasicTypeEnum::ArrayType(t) => t.const_zero().as_basic_value_enum(),
                    BasicTypeEnum::StructType(t) => t.const_zero().as_basic_value_enum(),
                    BasicTypeEnum::VectorType(t) => t.const_zero().as_basic_value_enum(),
                    BasicTypeEnum::ScalableVectorType(_) => {
                        unreachable!("scalable vectors are not used in this backend")
                    }
                };
                Some(zero)
            }

            Call { func, args } => {
                if func == "ok" {
                    if matches!(instr.ty, Type::Void) {
                        return Ok(None);
                    }

                    let ctx = self.codegen.context;
                    let dummy = match &instr.ty {
                        Type::Int => ctx.i32_type().const_int(0, false).as_basic_value_enum(),
                        Type::Bool => ctx.bool_type().const_int(0, false).as_basic_value_enum(),
                        Type::Char | Type::Byte => {
                            ctx.i8_type().const_int(0, false).as_basic_value_enum()
                        }
                        Type::String | Type::Array(_) => ctx
                            .i8_type()
                            .ptr_type(AddressSpace::from(0u16))
                            .const_null()
                            .as_basic_value_enum(),
                        Type::Named { .. } => {
                            ctx.i32_type().const_int(0, false).as_basic_value_enum()
                        }
                        Type::Void => unreachable!(),
                    };

                    return Ok(Some(dummy));
                }

                if func == "len" {
                    if args.len() != 1 {
                        return Err("builtin len() expects exactly 1 argument".into());
                    }

                    let arr_val = self.value_map.get(&args[0]).cloned().ok_or_else(|| {
                        format!("unknown value id {:?} in len() argument", args[0])
                    })?;

                    let arr_ptr = match arr_val {
                        BasicValueEnum::PointerValue(p) => p,
                        other => {
                            return Err(format!(
                                "len() expects pointer-valued array, got {:?}",
                                other
                            ));
                        }
                    };

                    let len_fn = self.get_runtime_array_len_int_fn();
                    let call_site = self
                        .codegen
                        .builder
                        .build_call(len_fn, &[arr_ptr.into()], "array_len_int")
                        .map_err(|e| e.to_string())?;

                    let len_val =
                        unsafe { BasicValueEnum::new(call_site.as_value_ref()) }.into_int_value();

                    Some(len_val.as_basic_value_enum())
                } else {
                    let mut llvm_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
                    for arg_id in args {
                        let v = self
                            .value_map
                            .get(arg_id)
                            .ok_or_else(|| format!("unknown value id {:?} in call args", arg_id))?;
                        llvm_args.push((*v).into());
                    }

                    let callee = if let Some(f) = self.lookup_runtime_callee(func) {
                        Some(f)
                    } else {
                        self.codegen.fn_map.get(func).cloned()
                    };

                    if let Some(callee) = callee {
                        let cs = self
                            .codegen
                            .builder
                            .build_call(callee, &llvm_args, "calltmp")
                            .map_err(|e| e.to_string())?;

                        let fn_ty = callee.get_type();
                        if fn_ty.get_return_type().is_none() {
                            None
                        } else {
                            let ret_val = unsafe { BasicValueEnum::new(cs.as_value_ref()) };
                            Some(ret_val)
                        }
                    } else {
                        if matches!(instr.ty, Type::Void) {
                            None
                        } else {
                            let ctx = self.codegen.context;
                            let dummy = match &instr.ty {
                                Type::Int => {
                                    ctx.i32_type().const_int(0, false).as_basic_value_enum()
                                }
                                Type::Bool => {
                                    ctx.bool_type().const_int(0, false).as_basic_value_enum()
                                }
                                Type::Char | Type::Byte => {
                                    ctx.i8_type().const_int(0, false).as_basic_value_enum()
                                }
                                Type::String | Type::Array(_) => ctx
                                    .i8_type()
                                    .ptr_type(AddressSpace::from(0u16))
                                    .const_null()
                                    .as_basic_value_enum(),
                                Type::Named { .. } => {
                                    ctx.i32_type().const_int(0, false).as_basic_value_enum()
                                }
                                Type::Void => unreachable!(),
                            };
                            Some(dummy)
                        }
                    }
                }
            }

            other => {
                return Err(format!(
                    "LLVM backend: instruction kind {:?} not yet supported",
                    other
                ));
            }
        };

        Ok(result)
    }

    fn get_runtime_array_new_int_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_array_new_int";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }

        let i32_ty = self.codegen.context.i32_type();
        let i8_ptr_ty = self
            .codegen
            .context
            .i8_type()
            .ptr_type(AddressSpace::from(0u16));
        let fn_ty = i8_ptr_ty.fn_type(&[i32_ty.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn lookup_runtime_callee(&self, func: &str) -> Option<FunctionValue<'ctx>> {
        match func {
            "printInt" => return Some(self.get_extern_print_int()),
            "printChar" => return Some(self.get_extern_print_char()),
            "printString" => return Some(self.get_extern_print_string()),
            // Any of these should map to the Rust runtime symbol 'log_logInfo'
            "logInfo" | "log_logInfo" => return Some(self.get_extern_log_log_info()),
            "runtime_read_file" => return Some(self.get_extern_runtime_read_file()),
            _ => {}
        }

        let separators: &[_] = &['.', ':'];
        let parts: Vec<&str> = func.split(separators).filter(|s| !s.is_empty()).collect();

        if parts.len() == 2 && parts[0] == "log" && parts[1] == "logInfo" {
            return Some(self.get_extern_log_log_info());
        }

        None
    }

    fn get_runtime_array_len_int_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_array_len_int";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }

        let i32_ty = self.codegen.context.i32_type();
        let i8_ptr_ty = self
            .codegen
            .context
            .i8_type()
            .ptr_type(AddressSpace::from(0u16));
        let fn_ty = i32_ty.fn_type(&[i8_ptr_ty.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_runtime_array_get_int_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_array_get_int";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }

        let i32_ty = self.codegen.context.i32_type();
        let i8_ptr_ty = self
            .codegen
            .context
            .i8_type()
            .ptr_type(AddressSpace::from(0u16));
        let fn_ty = i32_ty.fn_type(&[i8_ptr_ty.into(), i32_ty.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_runtime_array_set_int_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_array_set_int";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }

        let i32_ty = self.codegen.context.i32_type();
        let i8_ptr_ty = self
            .codegen
            .context
            .i8_type()
            .ptr_type(AddressSpace::from(0u16));
        let void_ty = self.codegen.context.void_type();
        let fn_ty = void_ty.fn_type(&[i8_ptr_ty.into(), i32_ty.into(), i32_ty.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn lower_terminator(&mut self, term: &Terminator) -> Result<(), String> {
        match term {
            Terminator::Return { value } => {
                let builder = self.codegen.builder;

                if let Some(vid) = value {
                    if let Some(v) = self.value_map.get(vid).cloned() {
                        builder.build_return(Some(&v)).map_err(|e| e.to_string())?;
                    } else {
                        let bt = self.codegen.llvm_type(&self.func.ret_ty);

                        let zero = match bt {
                            BasicTypeEnum::IntType(t) => {
                                t.const_int(0, false).as_basic_value_enum()
                            }
                            BasicTypeEnum::PointerType(t) => t.const_null().as_basic_value_enum(),
                            BasicTypeEnum::FloatType(t) => t.const_float(0.0).as_basic_value_enum(),
                            BasicTypeEnum::ArrayType(t) => t.const_zero().as_basic_value_enum(),
                            BasicTypeEnum::StructType(t) => t.const_zero().as_basic_value_enum(),
                            BasicTypeEnum::VectorType(t) => t.const_zero().as_basic_value_enum(),
                            BasicTypeEnum::ScalableVectorType(_) => {
                                unreachable!(
                                    "scalable vectors are not used as return types in this backend"
                                )
                            }
                        };

                        builder
                            .build_return(Some(&zero))
                            .map_err(|e| e.to_string())?;
                    }
                } else {
                    // 'return;' without value
                    if let Type::Void = self.func.ret_ty {
                        builder.build_return(None).map_err(|e| e.to_string())?;
                    } else {
                        let bt = self.codegen.llvm_type(&self.func.ret_ty);

                        let zero = match bt {
                            BasicTypeEnum::IntType(t) => {
                                t.const_int(0, false).as_basic_value_enum()
                            }
                            BasicTypeEnum::PointerType(t) => t.const_null().as_basic_value_enum(),
                            BasicTypeEnum::FloatType(t) => t.const_float(0.0).as_basic_value_enum(),
                            BasicTypeEnum::ArrayType(t) => t.const_zero().as_basic_value_enum(),
                            BasicTypeEnum::StructType(t) => t.const_zero().as_basic_value_enum(),
                            BasicTypeEnum::VectorType(t) => t.const_zero().as_basic_value_enum(),
                            BasicTypeEnum::ScalableVectorType(_) => {
                                unreachable!(
                                    "scalable vectors are not used as return types in this backend"
                                )
                            }
                        };

                        builder
                            .build_return(Some(&zero))
                            .map_err(|e| e.to_string())?;
                    }
                }
            }

            Terminator::Jump { target } => {
                let bb = *self
                    .bb_map
                    .get(target)
                    .ok_or_else(|| format!("unknown block id {:?} in jump", target))?;
                self.codegen
                    .builder
                    .build_unconditional_branch(bb)
                    .map_err(|e| e.to_string())?;
            }

            Terminator::Branch {
                cond,
                then_bb,
                else_bb,
            } => {
                let cond_val = self.expect_bool(*cond)?;
                let then_block = *self
                    .bb_map
                    .get(then_bb)
                    .ok_or_else(|| format!("unknown then_bb {:?}", then_bb))?;
                let else_block = *self
                    .bb_map
                    .get(else_bb)
                    .ok_or_else(|| format!("unknown else_bb {:?}", else_bb))?;

                self.codegen
                    .builder
                    .build_conditional_branch(cond_val, then_block, else_block)
                    .map_err(|e| e.to_string())?;
            }
        }

        Ok(())
    }

    fn get_or_create_alloca(
        &mut self,
        name: &str,
        ty: BasicTypeEnum<'ctx>,
    ) -> Result<PointerValue<'ctx>, String> {
        if let Some(p) = self.var_allocas.get(name) {
            return Ok(*p);
        }

        if let Some(first_instr) = self.entry_bb.get_first_instruction() {
            self.entry_builder.position_before(&first_instr);
        } else {
            self.entry_builder.position_at_end(self.entry_bb);
        }

        let alloca = self
            .entry_builder
            .build_alloca(ty, &format!("var_{}", name))
            .map_err(|e| e.to_string())?;
        self.var_allocas.insert(name.to_string(), alloca);
        Ok(alloca)
    }

    fn expect_int(&self, vid: ValueId) -> Result<IntValue<'ctx>, String> {
        let v = self
            .value_map
            .get(&vid)
            .ok_or_else(|| format!("expected int value, unknown ValueId {:?}", vid))?;

        match v {
            // Normal case: we already have an integer
            BasicValueEnum::IntValue(iv) => Ok(*iv),

            // Hacky case:
            // Sometimes we see a pointer (e.g. string literal) where IR expects an int.
            // Cast the pointer to i32 so BinOp / Branch / etc. can proceed.
            BasicValueEnum::PointerValue(pv) => {
                let i32_ty = self.codegen.context.i32_type();
                self.codegen
                    .builder
                    .build_ptr_to_int(*pv, i32_ty, "ptr_as_int")
                    .map_err(|e| e.to_string())
            }

            other => Err(format!("expected int value, got {:?}", other)),
        }
    }

    fn expect_bool(&self, vid: ValueId) -> Result<IntValue<'ctx>, String> {
        let iv = self.expect_int(vid)?;
        let ty = iv.get_type();

        if ty.get_bit_width() == 1 {
            Ok(iv)
        } else {
            let zero = ty.const_int(0, false);
            self.codegen
                .builder
                .build_int_compare(inkwell::IntPredicate::NE, iv, zero, "tobool")
                .map_err(|e| e.to_string())
        }
    }

    fn coerce_ints(
        &self,
        lhs: IntValue<'ctx>,
        rhs: IntValue<'ctx>,
    ) -> Result<(IntValue<'ctx>, IntValue<'ctx>), String> {
        let lhs_ty = lhs.get_type();
        let rhs_ty = rhs.get_type();
        let lhs_bw = lhs_ty.get_bit_width();
        let rhs_bw = rhs_ty.get_bit_width();

        if lhs_bw == rhs_bw {
            return Ok((lhs, rhs));
        }

        let builder = self.codegen.builder;

        if lhs_bw < rhs_bw {
            let lhs_ext = builder
                .build_int_z_extend(lhs, rhs_ty, "zext_lhs")
                .map_err(|e| e.to_string())?;
            Ok((lhs_ext, rhs))
        } else {
            let rhs_ext = builder
                .build_int_z_extend(rhs, lhs_ty, "zext_rhs")
                .map_err(|e| e.to_string())?;
            Ok((lhs, rhs_ext))
        }
    }
}
