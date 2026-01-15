use std::collections::HashMap;
use std::path::Path;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module as LlvmModule;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine};
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

struct CodeGen<'ctx, 'm> {
    ir: &'m IrModule,
    context: &'ctx Context,
    module: &'m LlvmModule<'ctx>,
    builder: &'m Builder<'ctx>,
    _target_machine: &'m TargetMachine,
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
            _target_machine: target_machine,
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
                self.llvm_type(&instr.ty).into()
            })
            .collect();

        let fn_ty = match func.ret_ty {
            Type::Void => self.context.void_type().fn_type(&param_tys, false),
            ref other => self.llvm_type(other).fn_type(&param_tys, false),
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

        let entry_bb = *bb_map
            .get(&func.entry)
            .unwrap_or_else(|| panic!("entry block {:?} missing", func.entry));

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
            let bb = *fcx.bb_map.get(&block.id).expect("missing bb in map");
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
                        let zero = fcx.dummy_for_type(other_ty);
                        self.builder.build_return(Some(&zero)).map_err(|e| e.to_string())?;
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
            Type::String | Type::Array(_) | Type::Named { .. } => self
                .context
                .i8_type()
                .ptr_type(AddressSpace::from(0u16))
                .as_basic_type_enum(),
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
    fn get_runtime_store_i32_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_store_i32";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }
        let ctx = self.codegen.context;
        let i8p = ctx.i8_type().ptr_type(AddressSpace::from(0u16));
        let i32 = ctx.i32_type();
        let fn_ty = ctx.void_type().fn_type(&[i8p.into(), i32.into(), i32.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_runtime_load_i32_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_load_i32";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }
        let ctx = self.codegen.context;
        let i8p = ctx.i8_type().ptr_type(AddressSpace::from(0u16));
        let i32 = ctx.i32_type();
        let fn_ty = i32.fn_type(&[i8p.into(), i32.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_runtime_array_new_int_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_array_new_int";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }
        let ctx = self.codegen.context;
        let i8p = ctx.i8_type().ptr_type(AddressSpace::from(0u16));
        let i32 = ctx.i32_type();
        let fn_ty = i8p.fn_type(&[i32.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_runtime_array_len_int_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_array_len_int";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }
        let ctx = self.codegen.context;
        let i8p = ctx.i8_type().ptr_type(AddressSpace::from(0u16));
        let i32 = ctx.i32_type();
        let fn_ty = i32.fn_type(&[i8p.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_runtime_array_get_int_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_array_get_int";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }
        let ctx = self.codegen.context;
        let i8p = ctx.i8_type().ptr_type(AddressSpace::from(0u16));
        let i32 = ctx.i32_type();
        let fn_ty = i32.fn_type(&[i8p.into(), i32.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_runtime_array_set_int_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_array_set_int";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }
        let ctx = self.codegen.context;
        let i8p = ctx.i8_type().ptr_type(AddressSpace::from(0u16));
        let i32 = ctx.i32_type();
        let fn_ty = ctx.void_type().fn_type(&[i8p.into(), i32.into(), i32.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_runtime_string_concat_fn(&self) -> FunctionValue<'ctx> {
        let name = "runtime_string_concat";
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }
        let ctx = self.codegen.context;
        let i8p = ctx.i8_type().ptr_type(AddressSpace::from(0u16));
        let fn_ty = i8p.fn_type(&[i8p.into(), i8p.into()], false);
        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn get_or_declare_extern(
        &self,
        name: &str,
        ret_ty: &Type,
        args: &[BasicMetadataValueEnum<'ctx>],
    ) -> FunctionValue<'ctx> {
        if let Some(f) = self.codegen.module.get_function(name) {
            return f;
        }

        let param_tys: Vec<BasicMetadataTypeEnum<'ctx>> = args
            .iter()
            .map(|a| {
                let bt: BasicTypeEnum<'ctx> = match *a {
                    BasicMetadataValueEnum::IntValue(v) => v.get_type().as_basic_type_enum(),
                    BasicMetadataValueEnum::PointerValue(v) => v.get_type().as_basic_type_enum(),
                    BasicMetadataValueEnum::FloatValue(v) => v.get_type().as_basic_type_enum(),
                    BasicMetadataValueEnum::StructValue(v) => v.get_type().as_basic_type_enum(),
                    BasicMetadataValueEnum::VectorValue(v) => v.get_type().as_basic_type_enum(),
                    BasicMetadataValueEnum::ArrayValue(v) => v.get_type().as_basic_type_enum(),
                    BasicMetadataValueEnum::ScalableVectorValue(v) => v.get_type().as_basic_type_enum(),
                    BasicMetadataValueEnum::MetadataValue(_) => self.codegen.context.i32_type().as_basic_type_enum(),
                };
                bt.into()
            })
            .collect();

        let fn_ty = match ret_ty {
            Type::Void => self.codegen.context.void_type().fn_type(&param_tys, false),
            other => self.codegen.llvm_type(other).fn_type(&param_tys, false),
        };

        self.codegen.module.add_function(name, fn_ty, None)
    }

    fn lower_instr(&mut self, instr: &Instr) -> Result<Option<BasicValueEnum<'ctx>>, String> {
        use InstrKind::*;

        let result = match &instr.kind {
            ConstInt(i) => Some(self.codegen.context.i32_type().const_int(*i as u64, true).as_basic_value_enum()),
            ConstBool(b) => Some(self.codegen.context.bool_type().const_int(if *b { 1 } else { 0 }, false).as_basic_value_enum()),
            ConstChar(ch) => Some(self.codegen.context.i8_type().const_int(*ch as u64, false).as_basic_value_enum()),

            ConstString(s) => {
                let name = format!(".str_{}", instr.id.0);
                let gv = self.codegen.builder.build_global_string_ptr(s, &name).map_err(|e| e.to_string())?;
                Some(gv.as_pointer_value().as_basic_value_enum())
            }

            Param { index } => {
                let v = self
                    .llvm_fn
                    .get_nth_param(*index as u32)
                    .ok_or_else(|| format!("param index {} out of bounds", index))?;
                Some(v)
            }

            BinOp { op, lhs, rhs } => {
                if matches!(instr.ty, Type::String) && matches!(op, IrBinOp::Add) {
                    let a = self.expect_ptr_i8(*lhs)?;
                    let b = self.expect_ptr_i8(*rhs)?;
                    let f = self.get_runtime_string_concat_fn();
                    let cs = self.codegen.builder.build_call(f, &[a.into(), b.into()], "strcat").map_err(|e| e.to_string())?;
                    let ret = unsafe { BasicValueEnum::new(cs.as_value_ref()) };
                    return Ok(Some(ret));
                }

                let b = self.codegen.builder;
                let val: IntValue<'ctx> = match op {
                    IrBinOp::And | IrBinOp::Or => {
                        let lhs_b = self.expect_bool(*lhs)?;
                        let rhs_b = self.expect_bool(*rhs)?;
                        match op {
                            IrBinOp::And => b.build_and(lhs_b, rhs_b, "andtmp").map_err(|e| e.to_string())?,
                            IrBinOp::Or => b.build_or(lhs_b, rhs_b, "ortmp").map_err(|e| e.to_string())?,
                            _ => unreachable!(),
                        }
                    }
                    _ => {
                        let lhs_v = self.expect_int(*lhs)?;
                        let rhs_v = self.expect_int(*rhs)?;
                        let (lhs_n, rhs_n) = self.coerce_ints(lhs_v, rhs_v)?;
                        match op {
                            IrBinOp::Add => b.build_int_add(lhs_n, rhs_n, "addtmp").map_err(|e| e.to_string())?,
                            IrBinOp::Sub => b.build_int_sub(lhs_n, rhs_n, "subtmp").map_err(|e| e.to_string())?,
                            IrBinOp::Mul => b.build_int_mul(lhs_n, rhs_n, "multmp").map_err(|e| e.to_string())?,
                            IrBinOp::Div => b.build_int_signed_div(lhs_n, rhs_n, "divtmp").map_err(|e| e.to_string())?,
                            IrBinOp::Eq => b.build_int_compare(inkwell::IntPredicate::EQ, lhs_n, rhs_n, "cmptmp").map_err(|e| e.to_string())?,
                            IrBinOp::NotEq => b.build_int_compare(inkwell::IntPredicate::NE, lhs_n, rhs_n, "cmptmp").map_err(|e| e.to_string())?,
                            IrBinOp::Lt => b.build_int_compare(inkwell::IntPredicate::SLT, lhs_n, rhs_n, "cmptmp").map_err(|e| e.to_string())?,
                            IrBinOp::LtEq => b.build_int_compare(inkwell::IntPredicate::SLE, lhs_n, rhs_n, "cmptmp").map_err(|e| e.to_string())?,
                            IrBinOp::Gt => b.build_int_compare(inkwell::IntPredicate::SGT, lhs_n, rhs_n, "cmptmp").map_err(|e| e.to_string())?,
                            IrBinOp::GtEq => b.build_int_compare(inkwell::IntPredicate::SGE, lhs_n, rhs_n, "cmptmp").map_err(|e| e.to_string())?,
                            IrBinOp::And | IrBinOp::Or => unreachable!(),
                        }
                    }
                };
                Some(val.as_basic_value_enum())
            }

            UnOp { op, value } => {
                let v = self.expect_int(*value)?;
                let b = self.codegen.builder;
                let res = match op {
                    IrUnOp::Neg => b.build_int_neg(v, "negtmp").map_err(|e| e.to_string())?,
                    IrUnOp::Not => b.build_not(v, "nottmp").map_err(|e| e.to_string())?,
                };
                Some(res.as_basic_value_enum())
            }

            StoreVar { name, value } => {
                let src = self.value_map.get(value).cloned().unwrap_or_else(|| self.dummy_for_type(&instr.ty));
                let alloca = self.get_or_create_alloca(name, src.get_type())?;
                self.codegen.builder.build_store(alloca, src).map_err(|e| e.to_string())?;
                None
            }

            LoadVar { name } => {
                if let Some(alloca) = self.var_allocas.get(name).cloned() {
                    let loaded = self.codegen.builder.build_load(alloca, &format!("load_{}", name)).map_err(|e| e.to_string())?;
                    Some(loaded)
                } else {
                    Some(self.dummy_for_type(&instr.ty))
                }
            }

            ArrayLit { elem_ty, elems } => match elem_ty {
                Type::Int => {
                    let i32_ty = self.codegen.context.i32_type();
                    let len_const = i32_ty.const_int(elems.len() as u64, false);

                    let new_fn = self.get_runtime_array_new_int_fn();
                    let cs = self.codegen.builder.build_call(new_fn, &[len_const.into()], "array_new_int").map_err(|e| e.to_string())?;
                    let arr_val = unsafe { BasicValueEnum::new(cs.as_value_ref()) }.into_pointer_value();

                    if !elems.is_empty() {
                        let set_fn = self.get_runtime_array_set_int_fn();
                        for (idx, elem_vid) in elems.iter().enumerate() {
                            let elem_val = self.expect_int(*elem_vid)?;
                            let idx_const = i32_ty.const_int(idx as u64, false);
                            self.codegen.builder.build_call(
                                set_fn,
                                &[arr_val.into(), idx_const.into(), elem_val.into()],
                                "array_set_int",
                            ).map_err(|e| e.to_string())?;
                        }
                    }

                    Some(arr_val.as_basic_value_enum())
                }
                other => return Err(format!("ArrayLit for element type {:?} not supported yet", other)),
            },

            Index { target, index } => {
                let arr_val = self.value_map.get(target).cloned().ok_or_else(|| format!("unknown value id {:?} in Index", target))?;
                let arr_ptr = match arr_val {
                    BasicValueEnum::PointerValue(p) => p,
                    other => return Err(format!("Index expects pointer-valued array, got {:?}", other)),
                };
                let idx_val = self.expect_int(*index)?;
                let get_fn = self.get_runtime_array_get_int_fn();
                let cs = self.codegen.builder.build_call(get_fn, &[arr_ptr.into(), idx_val.into()], "array_get_int").map_err(|e| e.to_string())?;
                let elem_val = unsafe { BasicValueEnum::new(cs.as_value_ref()) }.into_int_value();
                Some(elem_val.as_basic_value_enum())
            }

            Field { target, field_id } => {
                let obj = self.value_map.get(target).cloned().ok_or_else(|| format!("unknown value id {:?} in Field", target))?;
                let obj_ptr = match obj {
                    BasicValueEnum::PointerValue(p) => p,
                    other => return Err(format!("Field expects pointer target, got {:?}", other)),
                };

                let off = self.codegen.context.i32_type().const_int(*field_id as u64, true);
                let load_fn = self.get_runtime_load_i32_fn();
                let cs = self.codegen.builder.build_call(load_fn, &[obj_ptr.into(), off.into()], "load_i32").map_err(|e| e.to_string())?;
                let raw = unsafe { BasicValueEnum::new(cs.as_value_ref()) };
                Some(raw)
            }

            StoreField { target, field_id, value } => {
                let obj = self.value_map.get(target).cloned().ok_or_else(|| format!("unknown value id {:?} in StoreField", target))?;
                let obj_ptr = match obj {
                    BasicValueEnum::PointerValue(p) => p,
                    other => return Err(format!("StoreField expects pointer target, got {:?}", other)),
                };

                let val_any = self.value_map.get(value).cloned().ok_or_else(|| format!("unknown value id {:?} in StoreField value", value))?;
                let val_i32 = match val_any {
                    BasicValueEnum::IntValue(iv) => {
                        if iv.get_type().get_bit_width() == 32 {
                            iv
                        } else {
                            let i32t = self.codegen.context.i32_type();
                            self.codegen.builder.build_int_z_extend(iv, i32t, "sf_zext").map_err(|e| e.to_string())?
                        }
                    }
                    BasicValueEnum::PointerValue(pv) => {
                        let i32t = self.codegen.context.i32_type();
                        self.codegen.builder.build_ptr_to_int(pv, i32t, "sf_ptr_to_i32").map_err(|e| e.to_string())?
                    }
                    other => return Err(format!("StoreField value unsupported: {:?}", other)),
                };

                let off = self.codegen.context.i32_type().const_int(*field_id as u64, true);
                let store_fn = self.get_runtime_store_i32_fn();
                self.codegen.builder.build_call(
                    store_fn,
                    &[obj_ptr.into(), off.into(), val_i32.into()],
                    "store_i32",
                ).map_err(|e| e.to_string())?;
                None
            }

            StructLit { typ, .. } => {
                Some(self.dummy_for_type(typ))
            }

            Call { func, args } => {
                if func == "len" {
                    if args.len() != 1 {
                        return Err("builtin len() expects exactly 1 argument".into());
                    }
                    let arr_val = self.value_map.get(&args[0]).cloned().ok_or_else(|| format!("unknown value id {:?} in len()", args[0]))?;
                    let arr_ptr = match arr_val {
                        BasicValueEnum::PointerValue(p) => p,
                        other => return Err(format!("len() expects pointer-valued array, got {:?}", other)),
                    };
                    let len_fn = self.get_runtime_array_len_int_fn();
                    let cs = self.codegen.builder.build_call(len_fn, &[arr_ptr.into()], "array_len_int").map_err(|e| e.to_string())?;
                    let len_val = unsafe { BasicValueEnum::new(cs.as_value_ref()) }.into_int_value();
                    return Ok(Some(len_val.as_basic_value_enum()));
                }

                let mut llvm_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::with_capacity(args.len());
                for vid in args {
                    let v = self.value_map.get(vid).ok_or_else(|| format!("unknown value id {:?} in call args", vid))?;
                    llvm_args.push((*v).into());
                }

                let mut callee = self.codegen.fn_map.get(func).cloned();

                if callee.is_none() {
                    if let Some(last) = func.split('.').last() {
                        let matches: Vec<FunctionValue<'ctx>> = self.codegen.fn_map.iter().filter_map(|(k, v)| {
                            if k == last || k.ends_with(&format!(".{last}")) { Some(*v) } else { None }
                        }).collect();

                        if matches.len() == 1 {
                            callee = Some(matches[0]);
                        } else if matches.len() > 1 {
                            return Err(format!("ambiguous call '{func}': {} candidates for '{last}'", matches.len()));
                        }
                    }
                }

                let callee = match callee {
                    Some(f) => f,
                    None => match func.as_str() {
                        "runtime_store_i32" => self.get_runtime_store_i32_fn(),
                        "runtime_load_i32" => self.get_runtime_load_i32_fn(),
                        "runtime_array_new_int" => self.get_runtime_array_new_int_fn(),
                        "runtime_array_len_int" => self.get_runtime_array_len_int_fn(),
                        "runtime_array_get_int" => self.get_runtime_array_get_int_fn(),
                        "runtime_array_set_int" => self.get_runtime_array_set_int_fn(),
                        "runtime_string_concat" => self.get_runtime_string_concat_fn(),
                        _ => self.get_or_declare_extern(func, &instr.ty, &llvm_args),
                    },
                };

                let fn_ty = callee.get_type();
                let param_tys = fn_ty.get_param_types();
                if param_tys.len() != llvm_args.len() {
                    return Err(format!(
                        "call arity mismatch for '{}': expected {}, got {}",
                        func,
                        param_tys.len(),
                        llvm_args.len()
                    ));
                }

                let mut coerced_args: Vec<BasicMetadataValueEnum<'ctx>> = Vec::with_capacity(llvm_args.len());
                for (i, arg) in llvm_args.into_iter().enumerate() {
                    let expected = param_tys[i];

                    let actual_val: BasicValueEnum<'ctx> = match arg {
                        BasicMetadataValueEnum::IntValue(v) => v.as_basic_value_enum(),
                        BasicMetadataValueEnum::PointerValue(v) => v.as_basic_value_enum(),
                        BasicMetadataValueEnum::FloatValue(v) => v.as_basic_value_enum(),
                        BasicMetadataValueEnum::StructValue(v) => v.as_basic_value_enum(),
                        BasicMetadataValueEnum::VectorValue(v) => v.as_basic_value_enum(),
                        BasicMetadataValueEnum::ArrayValue(v) => v.as_basic_value_enum(),
                        BasicMetadataValueEnum::ScalableVectorValue(v) => v.as_basic_value_enum(),
                        BasicMetadataValueEnum::MetadataValue(_) => {
                            return Err(format!("call arg {} for '{}' is metadata-only (unsupported)", i, func));
                        }
                    };

                    let actual_ty = actual_val.get_type();

                    let coerced: BasicValueEnum<'ctx> = match (expected, actual_ty) {
                        (BasicMetadataTypeEnum::IntType(int_ty), BasicTypeEnum::PointerType(_)) => {
                            let pv = actual_val.into_pointer_value();
                            let iv = self.codegen.builder.build_ptr_to_int(pv, int_ty, "arg_ptr_to_int")
                                .map_err(|e: inkwell::builder::BuilderError| e.to_string())?;
                            iv.as_basic_value_enum()
                        }
                        (BasicMetadataTypeEnum::PointerType(ptr_ty), BasicTypeEnum::IntType(_)) => {
                            let iv = actual_val.into_int_value();
                            let pv = self.codegen.builder.build_int_to_ptr(iv, ptr_ty, "arg_int_to_ptr")
                                .map_err(|e: inkwell::builder::BuilderError| e.to_string())?;
                            pv.as_basic_value_enum()
                        }
                        (BasicMetadataTypeEnum::IntType(exp), BasicTypeEnum::IntType(_)) => {
                            let iv = actual_val.into_int_value();
                            let bw_exp = exp.get_bit_width();
                            let bw_got = iv.get_type().get_bit_width();
                            if bw_exp == bw_got {
                                iv.as_basic_value_enum()
                            } else if bw_got < bw_exp {
                                self.codegen.builder.build_int_z_extend(iv, exp, "arg_zext")
                                    .map_err(|e: inkwell::builder::BuilderError| e.to_string())?
                                    .as_basic_value_enum()
                            } else {
                                self.codegen.builder.build_int_truncate(iv, exp, "arg_trunc")
                                    .map_err(|e: inkwell::builder::BuilderError| e.to_string())?
                                    .as_basic_value_enum()
                            }
                        }
                        _ => actual_val,
                    };

                    coerced_args.push(coerced.into());
                }

                let cs = self.codegen.builder.build_call(callee, &coerced_args, "calltmp").map_err(|e| e.to_string())?;

                if callee.get_type().get_return_type().is_none() || matches!(instr.ty, Type::Void) {
                    None
                } else {
                    let ret_val = unsafe { BasicValueEnum::new(cs.as_value_ref()) };
                    Some(ret_val)
                }
            }
        };

        Ok(result)
    }

    fn dummy_for_type(&self, ty: &Type) -> BasicValueEnum<'ctx> {
        let ctx = self.codegen.context;
        match ty {
            Type::Int => ctx.i32_type().const_int(0, false).as_basic_value_enum(),
            Type::Bool => ctx.bool_type().const_int(0, false).as_basic_value_enum(),
            Type::Char | Type::Byte => ctx.i8_type().const_int(0, false).as_basic_value_enum(),
            Type::String | Type::Array(_) | Type::Named { .. } => ctx
                .i8_type()
                .ptr_type(AddressSpace::from(0u16))
                .const_null()
                .as_basic_value_enum(),
            Type::Void => ctx.i32_type().const_int(0, false).as_basic_value_enum(),
        }
    }

    fn lower_terminator(&mut self, term: &Terminator) -> Result<(), String> {
        match term {
            Terminator::Return { value } => {
                let b = self.codegen.builder;

                match value {
                    Some(vid) => {
                        if matches!(self.func.ret_ty, Type::Void) {
                            b.build_return(None).map_err(|e| e.to_string())?;
                            return Ok(());
                        }

                        let expected_bt = self.codegen.llvm_type(&self.func.ret_ty);
                        let mut v = self.value_map.get(vid).cloned().unwrap_or_else(|| self.dummy_for_type(&self.func.ret_ty));

                        v = match (expected_bt, v) {
                            (BasicTypeEnum::PointerType(pt), BasicValueEnum::IntValue(iv)) => {
                                let pv = b.build_int_to_ptr(iv, pt, "ret_int_to_ptr").map_err(|e: inkwell::builder::BuilderError| e.to_string())?;
                                pv.as_basic_value_enum()
                            }
                            (BasicTypeEnum::IntType(it), BasicValueEnum::PointerValue(pv)) => {
                                let iv = b.build_ptr_to_int(pv, it, "ret_ptr_to_int").map_err(|e: inkwell::builder::BuilderError| e.to_string())?;
                                iv.as_basic_value_enum()
                            }
                            _ => v,
                        };

                        b.build_return(Some(&v)).map_err(|e| e.to_string())?;
                    }

                    None => {
                        if matches!(self.func.ret_ty, Type::Void) {
                            b.build_return(None).map_err(|e| e.to_string())?;
                        } else {
                            let zero = self.dummy_for_type(&self.func.ret_ty);
                            b.build_return(Some(&zero)).map_err(|e| e.to_string())?;
                        }
                    }
                }
            }

            Terminator::Jump { target } => {
                let bb = *self.bb_map.get(target).ok_or_else(|| format!("unknown block id {:?} in jump", target))?;
                self.codegen.builder.build_unconditional_branch(bb).map_err(|e| e.to_string())?;
            }

            Terminator::Branch { cond, then_bb, else_bb } => {
                let cond_val = self.expect_bool(*cond)?;
                let then_block = *self.bb_map.get(then_bb).ok_or_else(|| format!("unknown then_bb {:?}", then_bb))?;
                let else_block = *self.bb_map.get(else_bb).ok_or_else(|| format!("unknown else_bb {:?}", else_bb))?;
                self.codegen.builder.build_conditional_branch(cond_val, then_block, else_block).map_err(|e| e.to_string())?;
            }
        }

        Ok(())
    }

    fn get_or_create_alloca(&mut self, name: &str, ty: BasicTypeEnum<'ctx>) -> Result<PointerValue<'ctx>, String> {
        if let Some(p) = self.var_allocas.get(name) {
            return Ok(*p);
        }

        if let Some(first_instr) = self.entry_bb.get_first_instruction() {
            self.entry_builder.position_before(&first_instr);
        } else {
            self.entry_builder.position_at_end(self.entry_bb);
        }

        let alloca = self.entry_builder.build_alloca(ty, &format!("var_{}", name)).map_err(|e| e.to_string())?;
        self.var_allocas.insert(name.to_string(), alloca);
        Ok(alloca)
    }

    fn expect_int(&self, vid: ValueId) -> Result<IntValue<'ctx>, String> {
        let v = self.value_map.get(&vid).ok_or_else(|| format!("expected int value, unknown ValueId {:?}", vid))?;
        match v {
            BasicValueEnum::IntValue(iv) => Ok(*iv),
            BasicValueEnum::PointerValue(pv) => {
                let i32_ty = self.codegen.context.i32_type();
                self.codegen.builder.build_ptr_to_int(*pv, i32_ty, "ptr_as_int").map_err(|e| e.to_string())
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
            self.codegen.builder.build_int_compare(inkwell::IntPredicate::NE, iv, zero, "tobool").map_err(|e| e.to_string())
        }
    }

    fn expect_ptr_i8(&self, vid: ValueId) -> Result<PointerValue<'ctx>, String> {
        let v = self.value_map.get(&vid).cloned().ok_or_else(|| format!("expected ptr value, unknown ValueId {:?}", vid))?;
        match v {
            BasicValueEnum::PointerValue(p) => Ok(p),
            BasicValueEnum::IntValue(iv) => {
                let i8p = self.codegen.context.i8_type().ptr_type(AddressSpace::from(0u16));
                self.codegen.builder.build_int_to_ptr(iv, i8p, "int_to_ptr").map_err(|e| e.to_string())
            }
            other => Err(format!("expected ptr value, got {:?}", other)),
        }
    }

    fn coerce_ints(&self, lhs: IntValue<'ctx>, rhs: IntValue<'ctx>) -> Result<(IntValue<'ctx>, IntValue<'ctx>), String> {
        let lhs_ty = lhs.get_type();
        let rhs_ty = rhs.get_type();
        let lhs_bw = lhs_ty.get_bit_width();
        let rhs_bw = rhs_ty.get_bit_width();

        if lhs_bw == rhs_bw {
            return Ok((lhs, rhs));
        }

        let b = self.codegen.builder;
        if lhs_bw < rhs_bw {
            let lhs_ext = b.build_int_z_extend(lhs, rhs_ty, "zext_lhs").map_err(|e| e.to_string())?;
            Ok((lhs_ext, rhs))
        } else {
            let rhs_ext = b.build_int_z_extend(rhs, lhs_ty, "zext_rhs").map_err(|e| e.to_string())?;
            Ok((lhs, rhs_ext))
        }
    }
}
