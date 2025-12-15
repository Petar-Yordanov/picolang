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

    Param {
        index: usize,
    },

    LoadVar {
        name: String,
    },

    StoreVar {
        name: String,
        value: ValueId,
    },

    BinOp {
        op: BinOp,
        lhs: ValueId,
        rhs: ValueId,
    },
    UnOp {
        op: UnOp,
        value: ValueId,
    },

    Call {
        func: String,
        args: Vec<ValueId>,
    },

    Index {
        target: ValueId,
        index: ValueId,
    },
    Field {
        target: ValueId,
        field: String,
    },

    StructLit {
        typ: Type,
        fields: Vec<(String, ValueId)>,
    },
    ArrayLit {
        elem_ty: Type,
        elems: Vec<ValueId>,
    },
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Return {
        value: Option<ValueId>,
    },

    Branch {
        cond: ValueId,
        then_bb: BlockId,
        else_bb: BlockId,
    },

    Jump {
        target: BlockId,
    },
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

#[derive(Debug)]
struct FnBuilder<'a> {
    func: IrFunction,
    next_value: usize,
    next_block: usize,
    cur_block: BlockId,
    locals: HashMap<String, Type>,
    loop_stack: Vec<LoopContext>,
    _module: &'a Module,
}

impl<'a> FnBuilder<'a> {
    fn new(module: &'a Module, name: &str, ret_ty: Type) -> Self {
        let entry = BlockId(0);

        let func = IrFunction {
            id: FuncId(0),
            name: name.to_string(),
            params: Vec::new(),
            ret_ty,
            blocks: vec![BasicBlock {
                id: entry,
                instrs: Vec::new(),
                term: None, // no terminator yet
            }],
            entry,
        };

        FnBuilder {
            func,
            next_value: 0,
            next_block: 1,
            cur_block: entry,
            locals: HashMap::new(),
            loop_stack: Vec::new(),
            _module: module,
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
            term: None, // no terminator yet
        });
        id
    }

    fn current_block_mut(&mut self) -> &mut BasicBlock {
        let idx = self.cur_block.0;
        &mut self.func.blocks[idx]
    }

    fn emit_instr(&mut self, ty: Type, kind: InstrKind) -> ValueId {
        let id = self.alloc_value();
        let instr = Instr { id, ty, kind };
        self.current_block_mut().instrs.push(instr);
        id
    }

    fn set_terminator(&mut self, term: Terminator) {
        self.current_block_mut().term = Some(term);
    }

    fn block_is_terminated(&self, bb: BlockId) -> bool {
        self.func.blocks[bb.0].term.is_some()
    }

    fn expr_is_string(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Literal(Literal::String(_)) => true,
            Expr::Path(p) if p.segments.len() == 1 => {
                let name = &p.segments[0];
                if let Some(ty) = self.locals.get(name) {
                    matches!(ty, Type::String)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn lower_block(&mut self, block: &Block, ret_ty: &Type) {
        for stmt in &block.stmts {
            self.lower_stmt(stmt, ret_ty);
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
                    InstrKind::StoreVar {
                        name: name.clone(),
                        value: value_id,
                    },
                );
            }

            Stmt::Expr(e) => {
                let _ = self.lower_expr(e);
            }

            Stmt::Return(eopt) => {
                let v = eopt.as_ref().map(|e| self.lower_expr(e));
                self.set_terminator(Terminator::Return { value: v });
            }

            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                let cond_val = self.lower_expr(cond);

                match else_branch {
                    Some(else_blk) => {
                        let then_bb = self.new_block();
                        let else_bb = self.new_block();
                        let merge_bb = self.new_block();

                        // branch from current block
                        self.set_terminator(Terminator::Branch {
                            cond: cond_val,
                            then_bb,
                            else_bb,
                        });

                        // then block
                        self.cur_block = then_bb;
                        self.lower_block(then_branch, ret_ty);
                        if !self.block_is_terminated(self.cur_block) {
                            self.set_terminator(Terminator::Jump { target: merge_bb });
                        }

                        // else block
                        self.cur_block = else_bb;
                        self.lower_block(else_blk, ret_ty);
                        if !self.block_is_terminated(self.cur_block) {
                            self.set_terminator(Terminator::Jump { target: merge_bb });
                        }

                        // continue in merge block
                        self.cur_block = merge_bb;
                    }

                    None => {
                        let then_bb = self.new_block();
                        let merge_bb = self.new_block();

                        // false path goes directly to merge
                        self.set_terminator(Terminator::Branch {
                            cond: cond_val,
                            then_bb,
                            else_bb: merge_bb,
                        });

                        // then block
                        self.cur_block = then_bb;
                        self.lower_block(then_branch, ret_ty);
                        if !self.block_is_terminated(self.cur_block) {
                            self.set_terminator(Terminator::Jump { target: merge_bb });
                        }

                        // continue in merge block
                        self.cur_block = merge_bb;
                    }
                }
            }

            Stmt::While { cond, body } => {
                let cond_bb = self.new_block();
                let body_bb = self.new_block();
                let after_bb = self.new_block();

                self.set_terminator(Terminator::Jump { target: cond_bb });

                self.loop_stack.push(LoopContext {
                    break_bb: after_bb,
                    continue_bb: cond_bb,
                });

                // cond block
                self.cur_block = cond_bb;
                let cond_val = self.lower_expr(cond);
                self.set_terminator(Terminator::Branch {
                    cond: cond_val,
                    then_bb: body_bb,
                    else_bb: after_bb,
                });

                // body
                self.cur_block = body_bb;
                self.lower_block(body, ret_ty);
                if !self.block_is_terminated(self.cur_block) {
                    self.set_terminator(Terminator::Jump { target: cond_bb });
                }

                self.loop_stack.pop();

                // continue after loop
                self.cur_block = after_bb;
            }

            Stmt::For {
                init,
                cond,
                step,
                body,
            } => {
                let cond_bb = self.new_block();
                let body_bb = self.new_block();
                let step_bb = self.new_block();
                let after_bb = self.new_block();

                if let Some(init_stmt) = init {
                    self.lower_stmt(init_stmt, ret_ty);
                }

                self.set_terminator(Terminator::Jump { target: cond_bb });

                self.loop_stack.push(LoopContext {
                    break_bb: after_bb,
                    continue_bb: step_bb,
                });

                self.cur_block = cond_bb;
                if let Some(c) = cond {
                    let v = self.lower_expr(c);
                    self.set_terminator(Terminator::Branch {
                        cond: v,
                        then_bb: body_bb,
                        else_bb: after_bb,
                    });
                } else {
                    let one = self.emit_instr(Type::Bool, InstrKind::ConstBool(true));
                    self.set_terminator(Terminator::Branch {
                        cond: one,
                        then_bb: body_bb,
                        else_bb: after_bb,
                    });
                }

                // body
                self.cur_block = body_bb;
                self.lower_block(body, ret_ty);
                if !self.block_is_terminated(self.cur_block) {
                    self.set_terminator(Terminator::Jump { target: step_bb });
                }

                // step
                self.cur_block = step_bb;
                if let Some(s) = step {
                    let _ = self.lower_expr(s);
                }
                if !self.block_is_terminated(self.cur_block) {
                    self.set_terminator(Terminator::Jump { target: cond_bb });
                }

                self.loop_stack.pop();

                // continue after loop
                self.cur_block = after_bb;
            }

            Stmt::Switch { .. } => {
                // TODO: Implement this, right now it's no-op
            }

            Stmt::Block(b) => {
                self.lower_block(b, ret_ty);
            }

            Stmt::Break => {
                if let Some(ctx) = self.loop_stack.last().copied() {
                    self.set_terminator(Terminator::Jump {
                        target: ctx.break_bb,
                    });
                } else {
                    // break outside a loop - no-op
                }
            }

            Stmt::Continue => {
                if let Some(ctx) = self.loop_stack.last().copied() {
                    self.set_terminator(Terminator::Jump {
                        target: ctx.continue_bb,
                    });
                } else {
                    // continue outside a loop - no-op
                }
            }
        }
    }

    fn lower_expr(&mut self, expr: &Expr) -> ValueId {
        match expr {
            Expr::Literal(Literal::Int(v)) => self.emit_instr(Type::Int, InstrKind::ConstInt(*v)),
            Expr::Literal(Literal::Bool(b)) => {
                self.emit_instr(Type::Bool, InstrKind::ConstBool(*b))
            }
            Expr::Literal(Literal::Char(c)) => {
                self.emit_instr(Type::Char, InstrKind::ConstChar(*c))
            }
            Expr::Literal(Literal::String(s)) => {
                self.emit_instr(Type::String, InstrKind::ConstString(s.clone()))
            }

            Expr::Path(p) => {
                if p.segments.len() == 1 {
                    let name = &p.segments[0];
                    if let Some(var_ty) = self.locals.get(name) {
                        self.emit_instr(var_ty.clone(), InstrKind::LoadVar { name: name.clone() })
                    } else {
                        self.emit_instr(Type::Int, InstrKind::LoadVar { name: name.clone() })
                    }
                } else {
                    let full = p.segments.join(".");
                    self.emit_instr(Type::Int, InstrKind::LoadVar { name: full })
                }
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

                self.emit_instr(
                    ty,
                    InstrKind::BinOp {
                        op: bin,
                        lhs: l,
                        rhs: r,
                    },
                )
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

                if let Expr::Path(p) = &**target {
                    if p.segments.len() == 1 {
                        let name = p.segments[0].clone();
                        let ty = self.locals.get(&name).cloned().unwrap_or(Type::Int);

                        self.locals.entry(name.clone()).or_insert(ty.clone());

                        self.emit_instr(ty, InstrKind::StoreVar { name, value: v });
                        v
                    } else {
                        // TODO: imlpement e.g. x.y = ... - leave for later
                        v
                    }
                } else {
                    v
                }
            }

            Expr::Call { callee, args } => {
                let func_name = match &**callee {
                    Expr::Path(p) => p.segments.last().cloned().unwrap_or_default(),
                    _ => "<anon>".to_string(),
                };
                let mut arg_values = Vec::new();
                for a in args {
                    arg_values.push(self.lower_expr(a));
                }

                self.emit_instr(
                    Type::Int,
                    InstrKind::Call {
                        func: func_name,
                        args: arg_values,
                    },
                )
            }

            Expr::Index { target, index } => {
                let t = self.lower_expr(target);
                let i = self.lower_expr(index);

                self.emit_instr(
                    Type::Int,
                    InstrKind::Index {
                        target: t,
                        index: i,
                    },
                )
            }

            Expr::Field { target, field } => {
                let t = self.lower_expr(target);
                self.emit_instr(
                    Type::Int,
                    InstrKind::Field {
                        target: t,
                        field: field.clone(),
                    },
                )
            }

            Expr::Question(inner) => {
                // For now, ignore "?" semantics
                self.lower_expr(inner)
            }

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
                    let v = self.lower_expr(e);
                    elems.push(v);
                }

                // TODO: Derive real element type from the type checker.
                // For now just use dummy element type
                let elem_ty = Type::Int;

                self.emit_instr(ty.clone(), InstrKind::ArrayLit { elem_ty, elems })
            }
        }
    }

    fn lower_default_value(&mut self, ty: Option<&Type>) -> ValueId {
        match ty {
            Some(Type::Bool) => self.emit_instr(Type::Bool, InstrKind::ConstBool(false)),
            Some(Type::Char) => self.emit_instr(Type::Char, InstrKind::ConstChar('\0')),
            Some(Type::Byte) => self.emit_instr(Type::Byte, InstrKind::ConstInt(0)),
            Some(Type::Int) | None => self.emit_instr(Type::Int, InstrKind::ConstInt(0)),
            Some(Type::String) => {
                self.emit_instr(Type::String, InstrKind::ConstString(String::new()))
            }
            Some(Type::Void) => self.emit_instr(Type::Int, InstrKind::ConstInt(0)),
            Some(Type::Array(inner)) => self.emit_instr(
                Type::Array(inner.clone()),
                InstrKind::ArrayLit {
                    elem_ty: Type::Int,
                    elems: Vec::new(),
                },
            ),
            Some(Type::Named { .. }) => {
                let t = ty.cloned().unwrap_or(Type::Int);
                self.emit_instr(
                    t.clone(),
                    InstrKind::StructLit {
                        typ: t,
                        fields: Vec::new(),
                    },
                )
            }
        }
    }
}

pub fn lower_to_ir(module: &Module) -> IrModule {
    let mut ir_mod = IrModule {
        functions: Vec::new(),
    };

    for (idx, item) in module.items.iter().enumerate() {
        if let Item::Function(f) = item {
            let mut builder = FnBuilder::new(module, &f.name, f.return_type.clone());

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

            // Lower body into blocks / instructions
            let ret_ty = f.return_type.clone();
            builder.lower_block(&f.body, &ret_ty);

            // Finalize: any block without a terminator gets a default 'return;'
            let mut func = builder.func;
            for bb in func.blocks.iter_mut() {
                if bb.term.is_none() {
                    bb.term = Some(Terminator::Return { value: None });
                }
            }

            func.id = FuncId(idx);
            ir_mod.functions.push(func);
        }
    }

    ir_mod
}

pub fn lower_modules_to_ir<'a, I>(modules: I) -> IrModule
where
    I: IntoIterator<Item = &'a Module>,
{
    let mut all_funcs = Vec::new();

    for m in modules {
        let mut m_ir = lower_to_ir(m);
        all_funcs.append(&mut m_ir.functions);
    }

    for (idx, f) in all_funcs.iter_mut().enumerate() {
        f.id = FuncId(idx);
    }

    IrModule {
        functions: all_funcs,
    }
}

pub fn lower_program_to_ir(program: &crate::module_loader::Program) -> IrModule {
    lower_modules_to_ir(program.modules.iter().map(|m| &m.ast))
}
