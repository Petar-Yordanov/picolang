use std::collections::HashMap;

use crate::ast::*;

#[derive(Debug, Clone)]
pub struct IrModule {
    pub functions: Vec<IrFunction>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(pub usize);

#[derive(Debug, Clone)]
pub struct IrFunction {
    pub id: FuncId,
    pub name: String,
    pub params: Vec<ValueId>,
    pub ret_ty: Type,
    pub blocks: Vec<BasicBlock>,
    pub entry: BlockId,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub id: BlockId,
    pub instrs: Vec<Instr>,
    pub term: Option<Terminator>,
}

#[derive(Debug, Clone)]
pub struct Instr {
    pub id: ValueId,
    pub ty: Type,
    pub kind: InstrKind,
}

#[derive(Debug, Clone)]
pub enum InstrKind {
    ConstInt(i64),
    ConstBool(bool),
    ConstChar(char),
    ConstString(String),

    Param { index: usize },

    LoadVar { name: String },
    StoreVar { name: String, value: ValueId },

    BinOp { op: BinOp, lhs: ValueId, rhs: ValueId },
    UnOp { op: UnOp, value: ValueId },

    Call { func: String, args: Vec<ValueId> },

    Index { target: ValueId, index: ValueId },

    Field { target: ValueId, field_id: i32 },
    StoreField { target: ValueId, field_id: i32, value: ValueId },

    StructLit { typ: Type, fields: Vec<(String, ValueId)> },
    ArrayLit { elem_ty: Type, elems: Vec<ValueId> },
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Return { value: Option<ValueId> },
    Branch { cond: ValueId, then_bb: BlockId, else_bb: BlockId },
    Jump { target: BlockId },
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
struct LoopContext {
    break_bb: BlockId,
    continue_bb: BlockId,
}

fn build_field_layouts(module: &Module, module_name: &str) -> HashMap<String, HashMap<String, i32>> {
    let mut layouts: HashMap<String, HashMap<String, i32>> = HashMap::new();

    for item in &module.items {
        let Item::Class(c) = item else { continue };

        let fq = format!("{}.{}", module_name, c.name);
        let mut m = HashMap::new();

        let slot: i32 = 4;

        for (i, f) in c.fields.iter().enumerate() {
            m.insert(f.name.clone(), (i as i32) * slot);
        }

        layouts.insert(fq, m);
    }

    layouts
}

#[derive(Debug)]
struct FnBuilder<'a> {
    func: IrFunction,
    next_value: usize,
    next_block: usize,
    cur_block: BlockId,

    locals: HashMap<String, Type>,
    loop_stack: Vec<LoopContext>,

    module_name: String,
    module: &'a Module,

    field_layouts: HashMap<String, HashMap<String, i32>>,
}

impl<'a> FnBuilder<'a> {
    fn new(module: &'a Module, module_name: &str, name: &str, ret_ty: Type) -> Self {
        let entry = BlockId(0);

        let func = IrFunction {
            id: FuncId(0),
            name: name.to_string(),
            params: Vec::new(),
            ret_ty,
            blocks: vec![BasicBlock {
                id: entry,
                instrs: Vec::new(),
                term: None,
            }],
            entry,
        };

        let field_layouts = build_field_layouts(module, module_name);

        Self {
            func,
            next_value: 0,
            next_block: 1,
            cur_block: entry,
            locals: HashMap::new(),
            loop_stack: Vec::new(),
            module_name: module_name.to_string(),
            module,
            field_layouts,
        }
    }

    fn alloc_value(&mut self) -> ValueId {
        let id = ValueId(self.next_value);
        self.next_value += 1;
        id
    }

    fn new_block(&mut self) -> BlockId {
        let id = BlockId(self.next_block);
        self.next_block += 1;
        self.func.blocks.push(BasicBlock {
            id,
            instrs: Vec::new(),
            term: None,
        });
        id
    }

    fn current_block_mut(&mut self) -> &mut BasicBlock {
        let idx = self.cur_block.0;
        &mut self.func.blocks[idx]
    }

    fn emit_instr(&mut self, ty: Type, kind: InstrKind) -> ValueId {
        let id = self.alloc_value();
        self.current_block_mut().instrs.push(Instr { id, ty, kind });
        id
    }

    fn set_terminator(&mut self, term: Terminator) {
        self.current_block_mut().term = Some(term);
    }

    fn block_is_terminated(&self, bb: BlockId) -> bool {
        self.func.blocks[bb.0].term.is_some()
    }

    fn fq_type_name(&self, ty: &Type) -> Option<String> {
        let Type::Named { name, .. } = ty else { return None };
        Some(if name.segments.len() == 1 {
            format!("{}.{}", self.module_name, name.segments[0])
        } else {
            name.segments.join(".")
        })
    }

    fn field_offset(&self, recv_ty: &Type, field: &str) -> Option<i32> {
        let fq = self.fq_type_name(recv_ty)?;
        self.field_layouts.get(&fq)?.get(field).copied()
    }

    fn expr_is_string(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(Literal::String(_)) => true,
            Expr::Path(p) if p.segments.len() == 1 => {
                let name = &p.segments[0];
                self.locals.get(name).map(|ty| matches!(ty, Type::String)).unwrap_or(false)
            }
            _ => false,
        }
    }

    fn is_local_function(&self, name: &str) -> bool {
        self.module.items.iter().any(|it| matches!(it, Item::Function(f) if f.name == name))
    }

    fn lower_block(&mut self, block: &Block, ret_ty: &Type) {
        for stmt in &block.stmts {
            self.lower_stmt(stmt, ret_ty);
        }
    }

    fn named_default_size_bytes(&self, ty: &Type) -> i32 {
        let Some(fq) = self.fq_type_name(ty) else { return 16 };
        let Some(layout) = self.field_layouts.get(&fq) else { return 16 };

        let mut max_off = 0i32;
        for off in layout.values().copied() {
            if off > max_off {
                max_off = off;
            }
        }

        let sz = max_off.saturating_add(4);
        if sz < 16 { 16 } else { sz }
    }

    fn lower_default_value(&mut self, ty: Option<&Type>) -> ValueId {
        match ty {
            Some(Type::Bool) => self.emit_instr(Type::Bool, InstrKind::ConstBool(false)),
            Some(Type::Char) => self.emit_instr(Type::Char, InstrKind::ConstChar('\0')),
            Some(Type::Byte) => self.emit_instr(Type::Byte, InstrKind::ConstInt(0)),
            Some(Type::Int) | None => self.emit_instr(Type::Int, InstrKind::ConstInt(0)),
            Some(Type::String) => self.emit_instr(Type::String, InstrKind::ConstString(String::new())),
            Some(Type::Void) => self.emit_instr(Type::Int, InstrKind::ConstInt(0)),

            Some(Type::Array(inner)) => self.emit_instr(
                Type::Array(inner.clone()),
                InstrKind::ArrayLit { elem_ty: Type::Int, elems: Vec::new() },
            ),

            Some(t @ Type::Named { .. }) => {
                let bytes = self.named_default_size_bytes(t);
                let sz = self.emit_instr(Type::Int, InstrKind::ConstInt(bytes as i64));
                self.emit_instr(
                    t.clone(),
                    InstrKind::Call {
                        func: "runtime_obj_alloc".to_string(),
                        args: vec![sz],
                    },
                )
            }
        }
    }

    fn lower_stmt(&mut self, stmt: &Stmt, ret_ty: &Type) {
        if self.block_is_terminated(self.cur_block) {
            return;
        }

        match stmt {
            Stmt::Let { name, ty, init } => {
                let var_ty = ty.clone().unwrap_or(Type::Int);
                self.locals.insert(name.clone(), var_ty.clone());

                let value_id = if let Some(expr) = init {
                    self.lower_expr(expr)
                } else {
                    self.lower_default_value(Some(&var_ty))
                };

                self.emit_instr(
                    var_ty,
                    InstrKind::StoreVar { name: name.clone(), value: value_id },
                );
            }

            Stmt::Expr(e) => { let _ = self.lower_expr(e); }

            Stmt::Return(eopt) => {
                let v = eopt.as_ref().map(|e| self.lower_expr(e));
                self.set_terminator(Terminator::Return { value: v });
            }

            Stmt::If { cond, then_branch, else_branch } => {
                let cond_val = self.lower_expr(cond);

                match else_branch {
                    Some(else_blk) => {
                        let then_bb = self.new_block();
                        let else_bb = self.new_block();
                        let merge_bb = self.new_block();

                        self.set_terminator(Terminator::Branch { cond: cond_val, then_bb, else_bb });

                        self.cur_block = then_bb;
                        self.lower_block(then_branch, ret_ty);
                        if !self.block_is_terminated(self.cur_block) {
                            self.set_terminator(Terminator::Jump { target: merge_bb });
                        }

                        self.cur_block = else_bb;
                        self.lower_block(else_blk, ret_ty);
                        if !self.block_is_terminated(self.cur_block) {
                            self.set_terminator(Terminator::Jump { target: merge_bb });
                        }

                        self.cur_block = merge_bb;
                    }

                    None => {
                        let then_bb = self.new_block();
                        let merge_bb = self.new_block();

                        self.set_terminator(Terminator::Branch {
                            cond: cond_val,
                            then_bb,
                            else_bb: merge_bb,
                        });

                        self.cur_block = then_bb;
                        self.lower_block(then_branch, ret_ty);
                        if !self.block_is_terminated(self.cur_block) {
                            self.set_terminator(Terminator::Jump { target: merge_bb });
                        }

                        self.cur_block = merge_bb;
                    }
                }
            }

            Stmt::While { cond, body } => {
                let cond_bb = self.new_block();
                let body_bb = self.new_block();
                let after_bb = self.new_block();

                self.set_terminator(Terminator::Jump { target: cond_bb });

                self.loop_stack.push(LoopContext { break_bb: after_bb, continue_bb: cond_bb });

                self.cur_block = cond_bb;
                let cond_val = self.lower_expr(cond);
                self.set_terminator(Terminator::Branch { cond: cond_val, then_bb: body_bb, else_bb: after_bb });

                self.cur_block = body_bb;
                self.lower_block(body, ret_ty);
                if !self.block_is_terminated(self.cur_block) {
                    self.set_terminator(Terminator::Jump { target: cond_bb });
                }

                self.loop_stack.pop();
                self.cur_block = after_bb;
            }

            Stmt::For { init, cond, step, body } => {
                let cond_bb = self.new_block();
                let body_bb = self.new_block();
                let step_bb = self.new_block();
                let after_bb = self.new_block();

                if let Some(init_stmt) = init {
                    self.lower_stmt(init_stmt, ret_ty);
                }

                self.set_terminator(Terminator::Jump { target: cond_bb });

                self.loop_stack.push(LoopContext { break_bb: after_bb, continue_bb: step_bb });

                self.cur_block = cond_bb;
                if let Some(c) = cond {
                    let v = self.lower_expr(c);
                    self.set_terminator(Terminator::Branch { cond: v, then_bb: body_bb, else_bb: after_bb });
                } else {
                    let one = self.emit_instr(Type::Bool, InstrKind::ConstBool(true));
                    self.set_terminator(Terminator::Branch { cond: one, then_bb: body_bb, else_bb: after_bb });
                }

                self.cur_block = body_bb;
                self.lower_block(body, ret_ty);
                if !self.block_is_terminated(self.cur_block) {
                    self.set_terminator(Terminator::Jump { target: step_bb });
                }

                self.cur_block = step_bb;
                if let Some(s) = step {
                    let _ = self.lower_expr(s);
                }
                if !self.block_is_terminated(self.cur_block) {
                    self.set_terminator(Terminator::Jump { target: cond_bb });
                }

                self.loop_stack.pop();
                self.cur_block = after_bb;
            }

            Stmt::Switch { .. } => {}

            Stmt::Block(b) => self.lower_block(b, ret_ty),

            Stmt::Break => {
                if let Some(ctx) = self.loop_stack.last().copied() {
                    self.set_terminator(Terminator::Jump { target: ctx.break_bb });
                }
            }

            Stmt::Continue => {
                if let Some(ctx) = self.loop_stack.last().copied() {
                    self.set_terminator(Terminator::Jump { target: ctx.continue_bb });
                }
            }
        }
    }

    fn hash32_fnv1a(s: &str) -> i32 {
        let mut h: u32 = 0x811c9dc5;
        for b in s.as_bytes() {
            h ^= *b as u32;
            h = h.wrapping_mul(16777619);
        }
        h as i32
    }

    fn looks_like_enum_ctor(path: &crate::ast::Path) -> bool {
        if path.segments.len() < 2 {
            return false;
        }
        let enum_name = &path.segments[path.segments.len() - 2];
        let var_name = &path.segments[path.segments.len() - 1];

        fn starts_upper(s: &str) -> bool {
            s.chars().next().map(|c| c.is_ascii_uppercase()).unwrap_or(false)
        }

        starts_upper(enum_name) && starts_upper(var_name)
    }

    fn lower_expr(&mut self, expr: &Expr) -> ValueId {
        match expr {
            Expr::Literal(Literal::Int(v)) => self.emit_instr(Type::Int, InstrKind::ConstInt(*v)),
            Expr::Literal(Literal::Bool(b)) => self.emit_instr(Type::Bool, InstrKind::ConstBool(*b)),
            Expr::Literal(Literal::Char(c)) => self.emit_instr(Type::Char, InstrKind::ConstChar(*c)),
            Expr::Literal(Literal::String(s)) => self.emit_instr(Type::String, InstrKind::ConstString(s.clone())),

            Expr::Path(p) => {
                if p.segments.len() == 1 {
                    let name = &p.segments[0];
                    let ty = self.locals.get(name).cloned().unwrap_or(Type::Int);
                    return self.emit_instr(ty, InstrKind::LoadVar { name: name.clone() });
                }

                if p.segments.len() == 2 && self.locals.contains_key(&p.segments[0]) {
                    let base = p.segments[0].clone();
                    let field = p.segments[1].clone();

                    let recv = self.lower_expr(&Expr::Path(Path {
                        segments: vec![base.clone()],
                        span: p.span,
                    }));

                    let recv_ty = self.locals.get(&base).cloned().unwrap_or(Type::Int);
                    let off = self
                        .field_offset(&recv_ty, &field)
                        .unwrap_or_else(|| 0);

                    return self.emit_instr(
                        Type::Int,
                        InstrKind::Field {
                            target: recv,
                            field_id: off,
                        },
                    );
                }

                let full = p.segments.join(".");
                self.emit_instr(Type::Int, InstrKind::LoadVar { name: full })
            }

            Expr::Binary { op, left, right } => {
                let l = self.lower_expr(left);
                let r = self.lower_expr(right);

                let bin = match op {
                    BinaryOp::Add => BinOp::Add,
                    BinaryOp::Sub => BinOp::Sub,
                    BinaryOp::Mul => BinOp::Mul,
                    BinaryOp::Div => BinOp::Div,
                    BinaryOp::Eq => BinOp::Eq,
                    BinaryOp::NotEq => BinOp::NotEq,
                    BinaryOp::Lt => BinOp::Lt,
                    BinaryOp::LtEq => BinOp::LtEq,
                    BinaryOp::Gt => BinOp::Gt,
                    BinaryOp::GtEq => BinOp::GtEq,
                    BinaryOp::And => BinOp::And,
                    BinaryOp::Or => BinOp::Or,
                };

                use BinaryOp::*;
                let ty = match op {
                    Add => {
                        if self.expr_is_string(left) || self.expr_is_string(right) {
                            Type::String
                        } else {
                            Type::Int
                        }
                    }
                    Sub | Mul | Div => Type::Int,
                    Eq | NotEq | Lt | LtEq | Gt | GtEq | And | Or => Type::Bool,
                };

                self.emit_instr(ty, InstrKind::BinOp { op: bin, lhs: l, rhs: r })
            }

            Expr::Unary { op, expr: inner } => {
                let v = self.lower_expr(inner);
                let un = match op {
                    UnaryOp::Neg => UnOp::Neg,
                    UnaryOp::Not => UnOp::Not,
                };
                let ty = match op {
                    UnaryOp::Neg => Type::Int,
                    UnaryOp::Not => Type::Bool,
                };
                self.emit_instr(ty, InstrKind::UnOp { op: un, value: v })
            }

            Expr::Assign { target, value } => {
                let v = self.lower_expr(value);

                match &**target {
                    Expr::Path(p) if p.segments.len() == 1 => {
                        let name = p.segments[0].clone();
                        let ty = self.locals.get(&name).cloned().unwrap_or(Type::Int);
                        self.locals.entry(name.clone()).or_insert(ty.clone());
                        self.emit_instr(ty, InstrKind::StoreVar { name, value: v });
                        v
                    }

                    Expr::Path(p) if p.segments.len() == 2 && self.locals.contains_key(&p.segments[0]) => {
                        let base = p.segments[0].clone();
                        let field = p.segments[1].clone();

                        let recv = self.lower_expr(&Expr::Path(Path {
                            segments: vec![base.clone()],
                            span: p.span,
                        }));

                        let recv_ty = self.locals.get(&base).cloned().unwrap_or(Type::Int);
                        let off = self.field_offset(&recv_ty, &field).unwrap_or(0);

                        self.emit_instr(
                            Type::Void,
                            InstrKind::StoreField {
                                target: recv,
                                field_id: off,
                                value: v,
                            },
                        );
                        v
                    }

                    Expr::Field { target: recv_expr, field } => {
                        let recv = self.lower_expr(recv_expr);

                        let recv_ty = match &**recv_expr {
                            Expr::Path(p) if p.segments.len() == 1 => self.locals.get(&p.segments[0]).cloned(),
                            _ => None,
                        }
                        .unwrap_or(Type::Int);

                        let off = self.field_offset(&recv_ty, field).unwrap_or(0);

                        self.emit_instr(
                            Type::Void,
                            InstrKind::StoreField {
                                target: recv,
                                field_id: off,
                                value: v,
                            },
                        );
                        v
                    }

                    _ => v,
                }
            }

            Expr::Call { callee, args } => {
                if let Expr::Field { target, field } = &**callee {
                    let recv = self.lower_expr(target);

                    let mut arg_values = Vec::with_capacity(args.len() + 1);
                    arg_values.push(recv);
                    for a in args {
                        arg_values.push(self.lower_expr(a));
                    }

                    let func_name = match &**target {
                        Expr::Path(p) if p.segments.len() == 1 => {
                            let var = &p.segments[0];
                            match self.locals.get(var) {
                                Some(Type::Named { name, .. }) => {
                                    let type_path = if name.segments.len() == 1 {
                                        format!("{}.{}", self.module_name, name.segments[0])
                                    } else {
                                        name.segments.join(".")
                                    };
                                    format!("{}.{}", type_path, field)
                                }
                                _ => field.clone(),
                            }
                        }
                        _ => field.clone(),
                    };

                    return self.emit_instr(
                        Type::Int,
                        InstrKind::Call {
                            func: func_name,
                            args: arg_values,
                        },
                    );
                }

                if let Expr::Path(p) = &**callee {
                    if Self::looks_like_enum_ctor(p) {
                        let n = args.len();
                        let size = 8 + 4 * n;

                        let sz = self.emit_instr(Type::Int, InstrKind::ConstInt(size as i64));

                        let full_variant = if p.segments.len() == 2 {
                            format!("{}.{}.{}", self.module_name, p.segments[0], p.segments[1])
                        } else {
                            p.segments.join(".")
                        };
                        let tag = Self::hash32_fnv1a(&full_variant);

                        let enum_segs = &p.segments[..p.segments.len() - 1];
                        let enum_ty_path = if enum_segs.len() == 1 {
                            vec![self.module_name.clone(), enum_segs[0].clone()]
                        } else {
                            enum_segs.to_vec()
                        };

                        let enum_ty = Type::Named {
                            name: Path { segments: enum_ty_path, span: p.span },
                            type_args: Vec::new(),
                        };

                        let obj = self.emit_instr(
                            enum_ty,
                            InstrKind::Call {
                                func: "runtime_obj_alloc".to_string(),
                                args: vec![sz],
                            },
                        );

                        let off0 = self.emit_instr(Type::Int, InstrKind::ConstInt(0));
                        let off4 = self.emit_instr(Type::Int, InstrKind::ConstInt(4));
                        let tagv = self.emit_instr(Type::Int, InstrKind::ConstInt(tag as i64));
                        let cntv = self.emit_instr(Type::Int, InstrKind::ConstInt(n as i64));

                        self.emit_instr(
                            Type::Void,
                            InstrKind::Call { func: "runtime_store_i32".to_string(), args: vec![obj, off0, tagv] },
                        );
                        self.emit_instr(
                            Type::Void,
                            InstrKind::Call { func: "runtime_store_i32".to_string(), args: vec![obj, off4, cntv] },
                        );

                        for (i, a) in args.iter().enumerate() {
                            let v = self.lower_expr(a);
                            let off = self.emit_instr(Type::Int, InstrKind::ConstInt((8 + 4 * i) as i64));
                            self.emit_instr(
                                Type::Void,
                                InstrKind::Call { func: "runtime_store_i32".to_string(), args: vec![obj, off, v] },
                            );
                        }

                        return obj;
                    }

                    if p.segments.len() == 2 && self.locals.contains_key(&p.segments[0]) {
                        let recv_var = p.segments[0].clone();
                        let method = p.segments[1].clone();

                        let recv = self.lower_expr(&Expr::Path(Path {
                            segments: vec![recv_var.clone()],
                            span: p.span,
                        }));

                        let mut arg_values = Vec::with_capacity(args.len() + 1);
                        arg_values.push(recv);
                        for a in args {
                            arg_values.push(self.lower_expr(a));
                        }

                        let func_name = match self.locals.get(&recv_var) {
                            Some(Type::Named { name, .. }) => {
                                let type_path = if name.segments.len() == 1 {
                                    format!("{}.{}", self.module_name, name.segments[0])
                                } else {
                                    name.segments.join(".")
                                };
                                format!("{}.{}", type_path, method)
                            }
                            _ => method,
                        };

                        return self.emit_instr(Type::Int, InstrKind::Call { func: func_name, args: arg_values });
                    }

                    let func_name = if p.segments.len() == 1 && self.is_local_function(&p.segments[0]) {
                        format!("{}.{}", self.module_name, p.segments[0])
                    } else {
                        p.segments.join(".")
                    };

                    let mut arg_values = Vec::with_capacity(args.len());
                    for a in args {
                        arg_values.push(self.lower_expr(a));
                    }

                    return self.emit_instr(Type::Int, InstrKind::Call { func: func_name, args: arg_values });
                }

                let mut arg_values = Vec::with_capacity(args.len());
                for a in args {
                    arg_values.push(self.lower_expr(a));
                }

                self.emit_instr(Type::Int, InstrKind::Call { func: "<anon>".to_string(), args: arg_values })
            }

            Expr::Index { target, index } => {
                let t = self.lower_expr(target);
                let i = self.lower_expr(index);
                self.emit_instr(Type::Int, InstrKind::Index { target: t, index: i })
            }

            Expr::Field { target, field } => {
                let recv = self.lower_expr(target);

                let recv_ty = match &**target {
                    Expr::Path(p) if p.segments.len() == 1 => self.locals.get(&p.segments[0]).cloned(),
                    _ => None,
                }
                .unwrap_or(Type::Int);

                let off = self.field_offset(&recv_ty, field).unwrap_or(0);

                self.emit_instr(Type::Int, InstrKind::Field { target: recv, field_id: off })
            }

            Expr::Question(inner) => self.lower_expr(inner),

            Expr::StructLiteral { typ, fields } => {
                let mut lowered_fields = Vec::new();
                for (name, expr) in fields {
                    let v = self.lower_expr(expr);
                    lowered_fields.push((name.clone(), v));
                }
                self.emit_instr(
                    typ.clone(),
                    InstrKind::StructLit {
                        typ: typ.clone(),
                        fields: lowered_fields,
                    },
                )
            }

            Expr::ArrayLiteral { ty, elements } => {
                let mut elems = Vec::new();
                for e in elements {
                    elems.push(self.lower_expr(e));
                }
                self.emit_instr(ty.clone(), InstrKind::ArrayLit { elem_ty: Type::Int, elems })
            }
        }
    }
}

pub fn lower_to_ir(module: &Module, module_name: &str) -> IrModule {
    let mut ir_mod = IrModule { functions: Vec::new() };

    for item in module.items.iter() {
        if let Item::Class(c) = item {
            for m in c.methods.iter() {
                let method_name = format!("{}.{}.{}", module_name, c.name, m.name);
                let mut builder =
                    FnBuilder::new(module, module_name, &method_name, m.return_type.clone());

                let this_ty = Type::Named {
                    name: Path {
                        segments: vec![module_name.to_string(), c.name.clone()],
                        span: c.name_span,
                    },
                    type_args: Vec::new(),
                };

                let this_vid = builder.emit_instr(this_ty.clone(), InstrKind::Param { index: 0 });
                builder.func.params.push(this_vid);
                builder.locals.insert("this".to_string(), this_ty.clone());
                builder.emit_instr(
                    this_ty.clone(),
                    InstrKind::StoreVar {
                        name: "this".to_string(),
                        value: this_vid,
                    },
                );

                for (pidx, p) in m.params.iter().enumerate() {
                    let vid = builder.emit_instr(p.ty.clone(), InstrKind::Param { index: pidx + 1 });
                    builder.func.params.push(vid);
                    builder.locals.insert(p.name.clone(), p.ty.clone());
                    builder.emit_instr(
                        p.ty.clone(),
                        InstrKind::StoreVar {
                            name: p.name.clone(),
                            value: vid,
                        },
                    );
                }

                let ret_ty = m.return_type.clone();
                builder.lower_block(&m.body, &ret_ty);

                let mut func = builder.func;
                for bb in func.blocks.iter_mut() {
                    if bb.term.is_none() {
                        bb.term = Some(Terminator::Return { value: None });
                    }
                }

                func.id = FuncId(ir_mod.functions.len());
                ir_mod.functions.push(func);
            }
        }

        if let Item::Function(f) = item {
            let name = if module_name == "main" && f.name == "main" {
                "main".to_string()
            } else {
                format!("{}.{}", module_name, f.name)
            };

            let mut builder = FnBuilder::new(module, module_name, &name, f.return_type.clone());

            for (param_idx, param) in f.params.iter().enumerate() {
                let param_vid =
                    builder.emit_instr(param.ty.clone(), InstrKind::Param { index: param_idx });
                builder.func.params.push(param_vid);
                builder.locals.insert(param.name.clone(), param.ty.clone());
                builder.emit_instr(
                    param.ty.clone(),
                    InstrKind::StoreVar {
                        name: param.name.clone(),
                        value: param_vid,
                    },
                );
            }

            let ret_ty = f.return_type.clone();
            builder.lower_block(&f.body, &ret_ty);

            let mut func = builder.func;
            for bb in func.blocks.iter_mut() {
                if bb.term.is_none() {
                    bb.term = Some(Terminator::Return { value: None });
                }
            }

            func.id = FuncId(ir_mod.functions.len());
            ir_mod.functions.push(func);
        }
    }

    ir_mod
}

pub fn lower_program_to_ir(program: &crate::module_loader::Program) -> IrModule {
    let mut all_funcs = Vec::new();

    for m in &program.modules {
        let mut one = lower_to_ir(&m.ast, &m.logical_name);
        all_funcs.append(&mut one.functions);
    }

    for (idx, f) in all_funcs.iter_mut().enumerate() {
        f.id = FuncId(idx);
    }

    IrModule { functions: all_funcs }
}
