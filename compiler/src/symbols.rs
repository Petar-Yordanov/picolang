use std::collections::HashMap;

use crate::ast::*;
use crate::diagnostics::{Diagnostic, Span};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Class,
    Enum,
    EnumVariant,
    Field,
    Param,
    Local,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    pub scope: ScopeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    pub symbols: HashMap<String, SymbolId>,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub symbols: Vec<Symbol>,
    pub scopes: Vec<Scope>,
    pub root_scope: ScopeId,
}

impl SymbolTable {
    fn new() -> Self {
        let root_scope = Scope {
            parent: None,
            symbols: HashMap::new(),
        };

        SymbolTable {
            symbols: Vec::new(),
            scopes: vec![root_scope],
            root_scope: ScopeId(0),
        }
    }

    fn new_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let id = ScopeId(self.scopes.len());
        self.scopes.push(Scope {
            parent,
            symbols: HashMap::new(),
        });
        id
    }

    fn insert_symbol(
        &mut self,
        scope: ScopeId,
        name: String,
        kind: SymbolKind,
        span: Span,
        diags: &mut Vec<Diagnostic>,
    ) -> SymbolId {
        if self.scopes[scope.0].symbols.contains_key(&name) {
            let msg = format!("redefinition of `{}` in this scope", name);
            diags.push(Diagnostic::new_error(msg, span));
        }

        let id = SymbolId(self.symbols.len());
        self.symbols.push(Symbol {
            name: name.clone(),
            kind,
            scope,
            span,
        });
        self.scopes[scope.0].symbols.insert(name, id);
        id
    }

    pub fn lookup_in_scope_chain(&self, mut scope: ScopeId, name: &str) -> Option<SymbolId> {
        loop {
            if let Some(id) = self.scopes[scope.0].symbols.get(name).copied() {
                return Some(id);
            }
            match self.scopes[scope.0].parent {
                Some(p) => scope = p,
                None => return None,
            }
        }
    }
}

pub fn build_symbol_table(module: &Module) -> (SymbolTable, Vec<Diagnostic>) {
    let mut symtab = SymbolTable::new();
    let mut diags = Vec::new();
    let root = symtab.root_scope;

    for item in &module.items {
        match item {
            Item::Function(f) => declare_function(&mut symtab, root, f, &mut diags),
            Item::Class(c) => declare_class(&mut symtab, root, c, &mut diags),
            Item::Enum(e) => declare_enum(&mut symtab, root, e, &mut diags),
            Item::Import(_) => {
                // imports are handled later by a real module system - for now ignore
            }
        }
    }

    check_module_types(module, &symtab, &mut diags);

    (symtab, diags)
}

fn declare_function(
    symtab: &mut SymbolTable,
    parent_scope: ScopeId,
    func: &Function,
    diags: &mut Vec<Diagnostic>,
) {
    let _func_id = symtab.insert_symbol(
        parent_scope,
        func.name.clone(),
        SymbolKind::Function,
        func.name_span,
        diags,
    );

    let fn_scope = symtab.new_scope(Some(parent_scope));
    for param in &func.params {
        symtab.insert_symbol(
            fn_scope,
            param.name.clone(),
            SymbolKind::Param,
            param.name_span,
            diags,
        );
    }
}

fn declare_class(
    symtab: &mut SymbolTable,
    parent_scope: ScopeId,
    class: &Class,
    diags: &mut Vec<Diagnostic>,
) {
    let _class_id = symtab.insert_symbol(
        parent_scope,
        class.name.clone(),
        SymbolKind::Class,
        class.name_span,
        diags,
    );

    let class_scope = symtab.new_scope(Some(parent_scope));

    for field in &class.fields {
        symtab.insert_symbol(
            class_scope,
            field.name.clone(),
            SymbolKind::Field,
            field.name_span,
            diags,
        );
    }

    for method in &class.methods {
        let _method_id = symtab.insert_symbol(
            class_scope,
            method.name.clone(),
            SymbolKind::Function,
            method.name_span,
            diags,
        );

        let method_scope = symtab.new_scope(Some(class_scope));
        for param in &method.params {
            symtab.insert_symbol(
                method_scope,
                param.name.clone(),
                SymbolKind::Param,
                param.name_span,
                diags,
            );
        }
    }
}

fn declare_enum(
    symtab: &mut SymbolTable,
    parent_scope: ScopeId,
    enm: &EnumDef,
    diags: &mut Vec<Diagnostic>,
) {
    let _enum_id = symtab.insert_symbol(
        parent_scope,
        enm.name.clone(),
        SymbolKind::Enum,
        enm.name_span,
        diags,
    );

    let enum_scope = symtab.new_scope(Some(parent_scope));
    for variant in &enm.variants {
        symtab.insert_symbol(
            enum_scope,
            variant.name.clone(),
            SymbolKind::EnumVariant,
            variant.name_span,
            diags,
        );
    }
}

fn check_module_types(module: &Module, symtab: &SymbolTable, diags: &mut Vec<Diagnostic>) {
    for item in &module.items {
        match item {
            Item::Function(f) => check_function_types(f, symtab, diags),
            Item::Class(c) => check_class_types(c, symtab, diags),
            Item::Enum(e) => check_enum_types(e, symtab, diags),
            Item::Import(_) => {}
        }
    }
}

fn check_function_types(func: &Function, symtab: &SymbolTable, diags: &mut Vec<Diagnostic>) {
    let type_params = func.type_params.clone();

    for param in &func.params {
        check_type(&param.ty, &type_params, symtab, diags);
    }
    check_type(&func.return_type, &type_params, symtab, diags);
}

fn check_class_types(class: &Class, symtab: &SymbolTable, diags: &mut Vec<Diagnostic>) {
    let class_tparams = class.type_params.clone();

    for field in &class.fields {
        check_type(&field.ty, &class_tparams, symtab, diags);
    }

    for method in &class.methods {
        let mut env = class_tparams.clone();
        env.extend(method.type_params.iter().cloned());

        for param in &method.params {
            check_type(&param.ty, &env, symtab, diags);
        }
        check_type(&method.return_type, &env, symtab, diags);
    }
}

fn check_enum_types(enm: &EnumDef, symtab: &SymbolTable, diags: &mut Vec<Diagnostic>) {
    let env = enm.type_params.clone();

    for variant in &enm.variants {
        for field_ty in &variant.fields {
            check_type(field_ty, &env, symtab, diags);
        }
    }
}

fn check_type(
    ty: &Type,
    type_params: &[String],
    symtab: &SymbolTable,
    diags: &mut Vec<Diagnostic>,
) {
    match ty {
        Type::Int | Type::Bool | Type::Void | Type::Char | Type::Byte | Type::String => {
            // built-in types
        }
        Type::Array(elem) => {
            check_type(elem, type_params, symtab, diags);
        }
        Type::Named { name, type_args } => {
            check_named_type(name, type_params, symtab, diags);
            for arg in type_args {
                check_type(arg, type_params, symtab, diags);
            }
        }
    }
}

fn check_named_type(
    path: &Path,
    type_params: &[String],
    symtab: &SymbolTable,
    diags: &mut Vec<Diagnostic>,
) {
    if path.segments.is_empty() {
        return;
    }

    if path.segments.len() > 1 {
        return;
    }

    let ident = &path.segments[0];

    if type_params.iter().any(|p| p == ident) {
        return;
    }

    if let Some(sym_id) = symtab.lookup_in_scope_chain(symtab.root_scope, ident) {
        let sym = &symtab.symbols[sym_id.0];
        match sym.kind {
            SymbolKind::Class | SymbolKind::Enum => {
                // real type
            }
            _ => {
                let msg = format!("`{}` is not a type (found {:?})", ident, sym.kind);
                diags.push(Diagnostic::new_error(msg, path.span));
            }
        }
    } else {
        let msg = format!("unknown type `{}`", ident);
        diags.push(Diagnostic::new_error(msg, path.span));
    }
}

#[derive(Debug, Clone)]
enum ExprType {
    Int,
    Bool,
    Void,
    Char,
    Byte,
    String,
    Array(Box<ExprType>),
    Named(String),
    Unknown,
}

fn expr_type_from_ast(t: &Type) -> ExprType {
    match t {
        Type::Int => ExprType::Int,
        Type::Bool => ExprType::Bool,
        Type::Void => ExprType::Void,
        Type::Char => ExprType::Char,
        Type::Byte => ExprType::Byte,
        Type::String => ExprType::String,
        Type::Array(elem) => ExprType::Array(Box::new(expr_type_from_ast(elem))),
        Type::Named { name, .. } => {
            if let Some(last) = name.segments.last() {
                ExprType::Named(last.clone())
            } else {
                ExprType::Unknown
            }
        }
    }
}

fn same_expr_type(a: &ExprType, b: &ExprType) -> bool {
    use ExprType::*;
    match (a, b) {
        (Int, Int)
        | (Bool, Bool)
        | (Void, Void)
        | (Char, Char)
        | (Byte, Byte)
        | (String, String) => true,

        (Array(ae), Array(be)) => same_expr_type(ae, be),

        (Named(na), Named(nb)) => na == nb,

        _ => false,
    }
}

#[derive(Debug, Clone)]
struct TypeEnv {
    vars: HashMap<String, ExprType>,
}

impl TypeEnv {
    fn new() -> Self {
        TypeEnv {
            vars: HashMap::new(),
        }
    }

    fn insert(&mut self, name: &str, ty: ExprType) {
        self.vars.insert(name.to_string(), ty);
    }

    fn get(&self, name: &str) -> Option<&ExprType> {
        self.vars.get(name)
    }
}

pub fn check_module_exprs(module: &Module, _symtab: &SymbolTable) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    for item in &module.items {
        match item {
            Item::Function(f) => check_function_exprs(f, &mut diags),
            Item::Class(c) => check_class_exprs(c, &mut diags),
            Item::Enum(_) | Item::Import(_) => {}
        }
    }

    diags
}

fn check_function_exprs(func: &Function, diags: &mut Vec<Diagnostic>) {
    let mut env = TypeEnv::new();

    for p in &func.params {
        env.insert(&p.name, expr_type_from_ast(&p.ty));
    }

    let expected = expr_type_from_ast(&func.return_type);
    check_block(&func.body, &mut env, Some(&expected), diags);
}

fn check_class_exprs(class: &Class, diags: &mut Vec<Diagnostic>) {
    for method in &class.methods {
        let mut env = TypeEnv::new();

        env.insert("this", ExprType::Unknown);

        for p in &method.params {
            env.insert(&p.name, expr_type_from_ast(&p.ty));
        }

        let expected = expr_type_from_ast(&method.return_type);
        check_block(&method.body, &mut env, Some(&expected), diags);
    }
}

fn check_block(
    block: &Block,
    env: &mut TypeEnv,
    expected_return: Option<&ExprType>,
    diags: &mut Vec<Diagnostic>,
) {
    for stmt in &block.stmts {
        check_stmt(stmt, env, expected_return, diags);
    }
}

fn check_stmt(
    stmt: &Stmt,
    env: &mut TypeEnv,
    expected_return: Option<&ExprType>,
    diags: &mut Vec<Diagnostic>,
) {
    match stmt {
        Stmt::Let { name, ty, init } => {
            let var_ty = if let Some(t) = ty {
                expr_type_from_ast(t)
            } else if let Some(e) = init {
                check_expr(e, env, diags)
            } else {
                ExprType::Unknown
            };

            env.insert(name, var_ty);
        }

        Stmt::Expr(e) => {
            let _ = check_expr(e, env, diags);
        }

        Stmt::Return(expr_opt) => match (expr_opt, expected_return) {
            (Some(expr), Some(exp_ty)) => {
                let got = check_expr(expr, env, diags);

                if !matches!(got, ExprType::Unknown)
                    && !matches!(exp_ty, ExprType::Unknown)
                    && !same_expr_type(exp_ty, &got)
                {
                    let msg = format!(
                        "return type mismatch: expected {:?}, found {:?}",
                        exp_ty, got
                    );
                    diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                }
            }
            (None, Some(exp_ty)) => {
                if !matches!(exp_ty, ExprType::Void | ExprType::Unknown) {
                    let msg = format!("return type mismatch: expected {:?}, found void", exp_ty);
                    diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                }
            }
            (Some(expr), None) => {
                let _ = check_expr(expr, env, diags);
            }
            (None, None) => {}
        },

        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_ty = check_expr(cond, env, diags);
            if !matches!(cond_ty, ExprType::Bool | ExprType::Unknown) {
                let msg = format!("condition of `if` must be bool, found {:?}", cond_ty);
                diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
            }

            check_block(then_branch, env, expected_return, diags);
            if let Some(else_blk) = else_branch {
                check_block(else_blk, env, expected_return, diags);
            }
        }

        Stmt::While { cond, body } => {
            let cond_ty = check_expr(cond, env, diags);
            if !matches!(cond_ty, ExprType::Bool | ExprType::Unknown) {
                let msg = format!("condition of `while` must be bool, found {:?}", cond_ty);
                diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
            }
            check_block(body, env, expected_return, diags);
        }

        Stmt::For {
            init,
            cond,
            step,
            body,
        } => {
            if let Some(init_stmt) = init {
                check_stmt(init_stmt, env, expected_return, diags);
            }

            if let Some(c) = cond {
                let cond_ty = check_expr(c, env, diags);
                if !matches!(cond_ty, ExprType::Bool | ExprType::Unknown) {
                    let msg = format!("condition of `for` must be bool, found {:?}", cond_ty);
                    diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                }
            }

            if let Some(s) = step {
                let _ = check_expr(s, env, diags);
            }

            check_block(body, env, expected_return, diags);
        }

        Stmt::Switch {
            expr,
            cases,
            default,
        } => {
            let _scrut_ty = check_expr(expr, env, diags);
            for c in cases {
                let _pat_ty = check_expr(&c.pattern, env, diags);
                check_block(&c.body, env, expected_return, diags);
            }

            if let Some(def) = default {
                check_block(def, env, expected_return, diags);
            }
        }

        Stmt::Block(b) => {
            check_block(b, env, expected_return, diags);
        }

        Stmt::Break | Stmt::Continue => {
            // no type checks
        }
    }
}

fn check_expr(expr: &Expr, env: &TypeEnv, diags: &mut Vec<Diagnostic>) -> ExprType {
    match expr {
        Expr::Literal(lit) => match lit {
            Literal::Int(_) => ExprType::Int,
            Literal::Bool(_) => ExprType::Bool,
            Literal::String(_) => ExprType::String,
            Literal::Char(_) => ExprType::Char,
        },

        Expr::Path(path) => {
            if path.segments.len() == 1 {
                let name = &path.segments[0];
                if let Some(ty) = env.get(name) {
                    return ty.clone();
                }
                ExprType::Unknown
            } else {
                ExprType::Unknown
            }
        }

        Expr::Binary { op, left, right } => {
            use BinaryOp::*;

            let lt = check_expr(left, env, diags);
            let rt = check_expr(right, env, diags);

            match op {
                Add => {
                    use ExprType::*;
                    match (&lt, &rt) {
                        (Int, Int) => Int,
                        (String, String) => String,
                        (Unknown, _) | (_, Unknown) => Unknown,
                        _ => {
                            let msg = format!(
                                "arithmetic operator {:?} requires (int, int) or (string, string), found ({:?}, {:?})",
                                op, lt, rt
                            );
                            diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                            Unknown
                        }
                    }
                }

                Sub | Mul | Div => {
                    use ExprType::*;
                    match (&lt, &rt) {
                        (Int, Int) => Int,
                        (Unknown, _) | (_, Unknown) => Unknown,
                        _ => {
                            let msg = format!(
                                "arithmetic operator {:?} requires (int, int), found ({:?}, {:?})",
                                op, lt, rt
                            );
                            diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                            Unknown
                        }
                    }
                }

                Lt | Gt | LtEq | GtEq => {
                    use ExprType::*;
                    match (&lt, &rt) {
                        (Int, Int) => Bool,
                        (Unknown, _) | (_, Unknown) => Unknown,
                        _ => {
                            let msg = format!(
                                "comparison operator {:?} requires (int, int), found ({:?}, {:?})",
                                op, lt, rt
                            );
                            diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                            Unknown
                        }
                    }
                }

                Eq | NotEq => {
                    use ExprType::*;

                    let both_numeric = matches!(lt, Int | Byte) && matches!(rt, Int | Byte);

                    if matches!(lt, Unknown) || matches!(rt, Unknown) {
                        ExprType::Unknown
                    } else if same_expr_type(&lt, &rt) || both_numeric {
                        ExprType::Bool
                    } else {
                        let msg = format!(
                            "equality operator {:?} requires both sides to have the same type, found ({:?}, {:?})",
                            op, lt, rt
                        );
                        diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                        ExprType::Unknown
                    }
                }

                And | Or => {
                    use ExprType::*;
                    match (&lt, &rt) {
                        (Bool, Bool) => Bool,
                        (Unknown, _) | (_, Unknown) => Unknown,
                        _ => {
                            let msg = format!(
                                "logical operator {:?} requires (bool, bool), found ({:?}, {:?})",
                                op, lt, rt
                            );
                            diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                            Unknown
                        }
                    }
                }
            }
        }

        Expr::Unary { op, expr: inner } => {
            let it = check_expr(inner, env, diags);
            match op {
                crate::ast::UnaryOp::Not => {
                    if matches!(it, ExprType::Bool) || matches!(it, ExprType::Unknown) {
                        ExprType::Bool
                    } else {
                        let msg = format!("logical `!` requires bool operand, found {:?}", it);
                        diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                        ExprType::Unknown
                    }
                }
                crate::ast::UnaryOp::Neg => {
                    if matches!(it, ExprType::Int) || matches!(it, ExprType::Unknown) {
                        ExprType::Int
                    } else {
                        let msg = format!("unary `-` requires int operand, found {:?}", it);
                        diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                        ExprType::Unknown
                    }
                }
            }
        }

        Expr::Assign { target, value } => {
            let vt = check_expr(value, env, diags);
            let tt = check_expr(target, env, diags);

            if !matches!(vt, ExprType::Unknown)
                && !matches!(tt, ExprType::Unknown)
                && !same_expr_type(&tt, &vt)
            {
                let msg = format!(
                    "cannot assign value of type {:?} to target of type {:?}",
                    vt, tt
                );
                diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
            }

            vt
        }

        Expr::Call { callee, args } => {
            let _callee_ty = check_expr(callee, env, diags);
            for arg in args {
                let _ = check_expr(arg, env, diags);
            }
            ExprType::Unknown
        }

        Expr::Index { target, index } => {
            let target_ty = check_expr(target, env, diags);
            let index_ty = check_expr(index, env, diags);

            if !matches!(index_ty, ExprType::Int) {
                let msg = format!("array index must be int, found {:?}", index_ty);
                diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
            }

            match target_ty {
                ExprType::Array(elem) => (*elem).clone(),

                ExprType::String => ExprType::Char,

                other => {
                    let msg = format!("cannot index into value of type {:?}", other);
                    diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                    ExprType::Int
                }
            }
        }

        Expr::Field { target, field: _ } => {
            let _ = check_expr(target, env, diags);
            ExprType::Unknown
        }

        Expr::Question(inner) => check_expr(inner, env, diags),

        Expr::StructLiteral { typ, fields } => {
            for (_, value) in fields {
                let _ = check_expr(value, env, diags);
            }
            expr_type_from_ast(typ)
        }

        Expr::ArrayLiteral { ty, elements } => {
            let arr_ty = expr_type_from_ast(ty);

            let elem_expected = match &arr_ty {
                ExprType::Array(elem) => &**elem,
                _ => &ExprType::Unknown,
            };

            for el in elements {
                let t = check_expr(el, env, diags);
                if !matches!(t, ExprType::Unknown)
                    && !matches!(elem_expected, ExprType::Unknown)
                    && !same_expr_type(elem_expected, &t)
                {
                    let msg = format!(
                        "array element has wrong type: expected {:?}, found {:?}",
                        elem_expected, t
                    );
                    diags.push(Diagnostic::new_error(msg, Span { start: 0, end: 0 }));
                }
            }

            arr_ty
        }
    }
}
