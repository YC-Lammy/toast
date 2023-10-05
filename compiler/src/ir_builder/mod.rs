use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;

use swc_common::Span;
use swc_common::Spanned;

use swc_atoms::JsWord;
use swc_ecmascript::ast::*;

use num_traits::cast::ToPrimitive;
use parking_lot::RwLock;

use serde::{Deserialize, Serialize};

use crate::error::Error;

use crate::parser::ParsedPackage;
use crate::Configuration;

use self::context::Variable;
use self::ir::ArgListId;
use self::ir::IterId;
use self::ir::TempId;
use self::ir::VarArgId;
use self::ir::IR;

mod assignment;
mod class;
mod context;
mod function;
mod function_builder;
pub mod ir;
mod ir_module;
//mod regex;

use class::*;
pub use function_builder::{FunctionId, IRFunction, IRFunctionBuilder};
use ir_module::IRModule;

pub struct IRBuilder {
    /// global configuration of the compiler
    config: Configuration,

    /// external declares : (name, id)
    declared_functions: RwLock<Vec<(JsWord, FunctionId)>>,

    functions: HashMap<FunctionId, IRFunction>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct IRPackage {
    pub global_variables: Vec<VariableId>,
    pub heap_variables: Vec<VariableId>,
    pub external_functions: Vec<(JsWord, FunctionId)>,
    pub functions: HashMap<FunctionId, IRFunction>,
    pub ir: Vec<IR>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct VariableId(uuid::Uuid);

impl VariableId {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

impl Display for VariableId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("VariableId({})", self.0))
    }
}

pub trait IRContainer: Send {
    fn push(&mut self, ir: IR);
    fn is_global(&self) -> bool;
    fn is_function(&self) -> bool {
        return false;
    }
    fn is_constructor(&self) -> bool {
        return false;
    }
    fn is_class_function(&self) -> Option<ClassID>;
    fn is_async(&self) -> bool;
    fn is_generator(&self) -> bool;

    /// internal use only, called from a IRFunctionBuilder to register a heap variable (capture)
    fn _add_heap_variable(&mut self, varid: VariableId);

    fn new_context(&mut self);
    fn close_context(&mut self);
    fn owns_variable(&self, varid: VariableId) -> bool;
    fn bind_variable(&mut self, name: &JsWord) -> VariableId;
    fn declare_anonymos(&mut self) -> VariableId;
    fn declare_var(&mut self, name: &JsWord) -> Result<VariableId, String>;
    fn declare_let(&mut self, name: &JsWord) -> Result<VariableId, String>;
    fn declare_const(&mut self, name: &JsWord) -> Result<VariableId, String>;
    fn declare_function(&mut self, name: &JsWord, funcid: FunctionId)
        -> Result<VariableId, String>;
    fn get_function(&self, name: &JsWord) -> (FunctionId, VariableId);
    fn declare_class(&mut self, name: &JsWord, classid: ClassID) -> Result<VariableId, String>;
    fn get_class(&self, name: &JsWord) -> (ClassID, VariableId);
    /// read the description of variable if exist
    fn read_var(&mut self, name: &JsWord) -> Option<Variable>;
}

impl IRBuilder {
    pub fn new(config: Configuration) -> Self {
        return Self {
            config: config,
            declared_functions: RwLock::new(Vec::new()),
            functions: HashMap::new(),
        };
    }

    pub fn build(mut self, parsed_package: ParsedPackage) -> Result<IRPackage, Error> {
        let mut module = IRModule::new();

        // resolve all the hoisting

        let mut stmts = Vec::new();

        // firstly var declares
        for item in parsed_package.module.body {
            match item {
                ModuleItem::ModuleDecl(_d) => {
                    unimplemented!()
                }
                ModuleItem::Stmt(s) => {
                    self.find_var_declares(&mut module, &s);
                    stmts.push(s);
                }
            }
        }

        // hoist declares
        self.hoist_declares(&mut module, &stmts)?;

        for stmt in &stmts {
            self.translate_stmt(&mut module, &stmt, None)?;
        }

        return Ok(IRPackage {
            global_variables: module.own_variable.into_iter().collect(),
            heap_variables: module.heap_variables,
            external_functions: self.declared_functions.into_inner(),
            functions: self.functions,
            ir: module.ir,
        });
    }

    /// a recussive function that finds all the 'var' declares in all scopes except functions
    pub fn find_var_declares(&self, container: &mut dyn IRContainer, stmt: &Stmt) {
        match stmt {
            Stmt::Block(b) => {
                for s in &b.stmts {
                    self.find_var_declares(container, s);
                }
            }
            Stmt::Break(_) => {}
            Stmt::Continue(_) => {}
            Stmt::Debugger(_) => {}
            Stmt::Decl(d) => match d {
                Decl::Var(v) => {
                    if v.kind == VarDeclKind::Var {
                        for d in &v.decls {
                            let names = self.variable_names_from_pat(&d.name);
                            names.iter().for_each(|name| {
                                container
                                    .declare_var(name)
                                    .expect("var declare sequence incorrect");
                            });
                        }
                    }
                }
                _ => {}
            },
            Stmt::DoWhile(d) => {
                self.find_var_declares(container, &d.body);
            }
            Stmt::Empty(_) => {}
            Stmt::Expr(_) => {}
            Stmt::For(f) => {
                if let Some(init) = &f.init {
                    match init {
                        VarDeclOrExpr::Expr(_) => {}
                        VarDeclOrExpr::VarDecl(v) => {
                            if v.kind == VarDeclKind::Var {
                                for d in &v.decls {
                                    let names = self.variable_names_from_pat(&d.name);
                                    names.iter().for_each(|name| {
                                        container
                                            .declare_var(name)
                                            .expect("var declare sequence incorrect");
                                    });
                                }
                            }
                        }
                    }
                };

                self.find_var_declares(container, &f.body);
            }
            Stmt::ForIn(f) => {
                match &f.left {
                    ForHead::VarDecl(v) => {
                        if v.kind == VarDeclKind::Var {
                            for d in &v.decls {
                                let names = self.variable_names_from_pat(&d.name);
                                names.iter().for_each(|name| {
                                    container
                                        .declare_var(name)
                                        .expect("var declare sequence incorrect");
                                });
                            }
                        }
                    }
                    _ => {}
                };

                self.find_var_declares(container, &f.body);
            }

            Stmt::ForOf(f) => {
                match &f.left {
                    ForHead::VarDecl(v) => {
                        if v.kind == VarDeclKind::Var {
                            for d in &v.decls {
                                let names = self.variable_names_from_pat(&d.name);
                                names.iter().for_each(|name| {
                                    container
                                        .declare_var(name)
                                        .expect("var declare sequence incorrect");
                                });
                            }
                        }
                    }
                    _ => {}
                };

                self.find_var_declares(container, &f.body);
            }
            Stmt::If(f) => {
                self.find_var_declares(container, &f.cons);
                if let Some(alt) = &f.alt {
                    self.find_var_declares(container, &alt);
                }
            }
            Stmt::Labeled(l) => {
                self.find_var_declares(container, &l.body);
            }
            Stmt::Return(_) => {}
            Stmt::Switch(s) => {
                for c in &s.cases {
                    c.cons
                        .iter()
                        .for_each(|s| self.find_var_declares(container, s));
                }
            }
            Stmt::Throw(_) => {}
            Stmt::Try(t) => {
                for s in &t.block.stmts {
                    self.find_var_declares(container, s);
                }

                if let Some(handler) = &t.handler {
                    for s in &handler.body.stmts {
                        self.find_var_declares(container, s);
                    }
                }

                if let Some(finaliser) = &t.finalizer {
                    for s in &finaliser.stmts {
                        self.find_var_declares(container, s);
                    }
                }
            }
            Stmt::While(w) => {
                self.find_var_declares(container, &w.body);
            }
            Stmt::With(w) => {
                self.find_var_declares(container, &w.body);
            }
        }
    }

    /// find all the declarations in the current scope
    ///
    /// call this function every time a new context is opened
    pub fn hoist_declares(
        &self,
        container: &mut dyn IRContainer,
        stmts: &[Stmt],
    ) -> Result<(), Error> {
        let form_err = |err: Result<_, String>, span: Span| -> Result<(), Error> {
            match err {
                Ok(_) => Ok(()),
                Err(e) => Err(Error::new(span, e, "")),
            }
        };

        for stmt in stmts {
            match stmt {
                Stmt::Decl(d) => {
                    match d {
                        Decl::Class(c) => {
                            let re = container.declare_class(&c.ident.sym, ClassID::new());
                            form_err(re, c.ident.span)?;
                        }
                        Decl::Fn(f) => {
                            let re = container.declare_function(&f.ident.sym, FunctionId::new());
                            form_err(re, f.ident.span)?;
                        }
                        Decl::Var(v) => {
                            if v.kind == VarDeclKind::Let {
                                for d in &v.decls {
                                    let names = self.variable_names_from_pat(&d.name);

                                    for name in &names {
                                        let re = container.declare_let(name);
                                        form_err(re, d.span)?;
                                    }
                                }
                            }

                            if v.kind == VarDeclKind::Const {
                                for d in &v.decls {
                                    let names = self.variable_names_from_pat(&d.name);

                                    for name in &names {
                                        let re = container.declare_const(name);
                                        form_err(re, d.span)?;
                                    }
                                }
                            };
                        }
                        _ => {}
                    };
                }
                _ => {}
            };
        }
        return Ok(());
    }

    pub fn variable_names_from_pat(&self, pat: &Pat) -> Vec<JsWord> {
        let mut names = Vec::new();

        match pat {
            Pat::Array(a) => {
                for p in &a.elems {
                    if let Some(p) = p {
                        names.extend_from_slice(&self.variable_names_from_pat(p));
                    }
                }
            }
            Pat::Assign(a) => {
                names.extend(self.variable_names_from_pat(&a.left));
            }
            Pat::Expr(_) => {}
            Pat::Ident(i) => {
                names.push(i.sym.clone());
            }
            Pat::Invalid(_) => {}
            Pat::Object(o) => {
                for prop in &o.props {
                    match prop {
                        ObjectPatProp::Assign(a) => {
                            names.push(a.key.sym.clone());
                        }
                        ObjectPatProp::KeyValue(k) => {
                            names.extend(self.variable_names_from_pat(&k.value))
                        }
                        ObjectPatProp::Rest(r) => {
                            names.extend(self.variable_names_from_pat(&r.arg))
                        }
                    }
                }
            }
            Pat::Rest(r) => {
                names.extend(self.variable_names_from_pat(&r.arg));
            }
        }
        return names;
    }

    pub fn translate_stmt(
        &mut self,
        container: &mut dyn IRContainer,
        stmt: &Stmt,
        label: Option<JsWord>,
    ) -> Result<(), Error> {
        match stmt {
            Stmt::Block(b) => {
                container.new_context();
                self.hoist_declares(container, &b.stmts)?;

                for s in &b.stmts {
                    self.translate_stmt(container, s, None)?;
                }

                container.close_context();
            }
            Stmt::Break(b) => {
                container.push(IR::Break {
                    label: b.label.as_ref().map(|i| i.sym.clone()),
                });
            }
            Stmt::Continue(c) => container.push(IR::Continue {
                label: c.label.as_ref().map(|i| i.sym.clone()),
            }),
            Stmt::Debugger(d) => {
                if !self.config.release {
                    container.push(IR::Debugger);
                } else {
                    return Err(Error::new(
                        d.span,
                        "debugger statment only allowed in debug builds.",
                        "",
                    ));
                }
            }
            Stmt::Decl(d) => {
                self.translate_decl(container, d)?;
            }
            Stmt::Empty(_) => {}
            Stmt::Expr(e) => {
                self.translate_expr(container, &e.expr)?;
            }
            Stmt::If(i) => {
                self.translate_expr(container, &i.test)?;

                container.new_context();

                if i.alt.is_none() {
                    container.push(IR::If);
                } else {
                    container.push(IR::IfElse);
                }
                self.translate_stmt(container, &i.cons, None)?;
                container.push(IR::EndIf);

                container.close_context();

                if let Some(alt) = &i.alt {
                    container.new_context();

                    self.translate_stmt(container, alt, None)?;
                    container.push(IR::EndElse);

                    container.close_context();
                }
            }
            Stmt::Labeled(l) => {
                container.push(IR::Block {
                    label: l.label.sym.clone(),
                });

                self.translate_stmt(container, &l.body, Some(l.label.sym.clone()))?;

                container.push(IR::EndBlock);
            }
            Stmt::Return(r) => {
                if let Some(arg) = &r.arg {
                    self.translate_expr(container, arg)?;
                } else {
                    container.push(IR::LoadUndefined)
                }
                container.push(IR::Return);
            }
            Stmt::Throw(t) => {
                self.translate_expr(container, &t.arg)?;
                container.push(IR::Throw);
            }
            Stmt::Try(t) => {
                container.new_context();
                container.push(IR::TryCatch);

                self.hoist_declares(container, &t.block.stmts)?;
                for stmt in &t.block.stmts {
                    self.translate_stmt(container, stmt, None)?;
                }

                container.push(IR::EndTry);

                container.close_context();

                if let Some(catch) = &t.handler {
                    container.new_context();

                    if let Some(p) = &catch.param {
                        let names = self.variable_names_from_pat(p);
                        for name in &names {
                            container.bind_variable(name);
                        }

                        // error is stored in ACC
                        self.translate_assignment(container, p, None)?;
                    }

                    self.hoist_declares(container, &catch.body.stmts)?;

                    for stmt in &catch.body.stmts {
                        self.translate_stmt(container, stmt, None)?;
                    }

                    container.close_context();
                }
                container.push(IR::EndCatch);

                if let Some(f) = &t.finalizer {
                    container.new_context();

                    self.hoist_declares(container, &f.stmts)?;

                    for stmt in &f.stmts {
                        self.translate_stmt(container, stmt, None)?;
                    }

                    container.close_context();
                }

                container.push(IR::EndTryCatchFinalizer);
            }
            Stmt::DoWhile(d) => {
                container.new_context();

                container.push(IR::Loop { label });
                self.translate_stmt(container, &d.body, None)?;
                self.translate_expr(container, &d.test)?;
                container.push(IR::BreakIfFalse);
                container.push(IR::EndLoop);

                container.close_context();
            }
            Stmt::While(w) => {
                container.new_context();

                container.push(IR::Loop { label });

                // translate the test value
                self.translate_expr(container, &w.test)?;
                // break if the test value is false
                container.push(IR::BreakIfFalse);
                // execute the body
                self.translate_stmt(container, &w.body, None)?;
                container.push(IR::EndLoop);

                container.close_context();
            }
            Stmt::For(f) => {
                container.new_context();

                if let Some(init) = &f.init {
                    match init {
                        VarDeclOrExpr::VarDecl(v) => {
                            if v.kind != VarDeclKind::Var {
                                for d in &v.decls {
                                    let names = self.variable_names_from_pat(&d.name);
                                    for name in &names {
                                        let re = if v.kind == VarDeclKind::Let {
                                            container.declare_let(name)
                                        } else {
                                            container.declare_const(name)
                                        };

                                        if let Err(e) = re {
                                            return Err(Error::new(d.span, e, ""));
                                        }
                                    }
                                }
                            };
                            self.translate_vardecl(container, v)?;
                        }
                        VarDeclOrExpr::Expr(e) => {
                            self.translate_expr(container, e)?;
                        }
                    }
                }

                container.push(IR::Loop { label });

                if let Some(test) = &f.test {
                    self.translate_expr(container, &test)?;
                    container.push(IR::BreakIfFalse);
                }

                self.translate_stmt(container, &f.body, None)?;

                if let Some(update) = &f.update {
                    self.translate_expr(container, &update)?;
                }

                container.push(IR::EndLoop);

                container.close_context();
            }
            Stmt::ForIn(f) => {
                container.new_context();

                self.translate_expr(container, &f.right)?;
                let iterid = IterId::new();
                container.push(IR::CreateForInIter(iterid));

                container.push(IR::Loop { label });
                // break if iterator finishes
                container.push(IR::BreakIfIterDone(iterid));
                // load next value of iterator to ACC
                container.push(IR::IterNext(iterid));

                match &f.left {
                    ForHead::Pat(p) => {
                        // assign directly to pattern
                        self.translate_assignment(container, p, None)?;
                    }
                    ForHead::VarDecl(v) => {
                        if v.decls.len() != 1 {
                            return Err(Error::new(v.span, "Invalid left-hand side in for-in loop: Must have a single binding.", ""));
                        }

                        let decl = &v.decls[0];

                        if decl.init.is_some() {
                            return Err(Error::new(
                                decl.span,
                                "for-in loop variable declaration may not have an initializer.",
                                "",
                            ));
                        }

                        // declare varables for the left hand name
                        let names = self.variable_names_from_pat(&decl.name);
                        for name in &names {
                            let re = match v.kind {
                                VarDeclKind::Const => container.declare_const(name).err(),
                                VarDeclKind::Let => container.declare_let(name).err(),
                                // var declare should be hoisted already
                                VarDeclKind::Var => None,
                            };
                            if let Some(err) = re {
                                return Err(Error::new(decl.span, err, ""));
                            }
                        }

                        // assign next value to pattern
                        self.translate_assignment(container, &decl.name, Some(v.kind))?;
                    }
                    ForHead::UsingDecl(_u) => {
                        todo!()
                    }
                };

                // execute the body
                self.translate_stmt(container, &f.body, None)?;

                container.push(IR::EndLoop);
                container.push(IR::DropIterator(iterid));

                container.close_context();
            }
            Stmt::ForOf(f) => {
                if f.is_await && !container.is_async() {
                    return Err(Error::new(
                        f.span,
                        "for await...of loop is only allowed in async context.",
                        "",
                    ));
                }

                container.new_context();

                self.translate_expr(container, &f.right)?;

                let iterid = IterId::new();

                if f.is_await {
                    container.push(IR::CreateAsyncIter(iterid));
                } else {
                    container.push(IR::CreateForOfIter(iterid));
                }

                container.push(IR::Loop { label });
                // break if iterator finishes
                container.push(IR::BreakIfIterDone(iterid));
                // load next value of iterator to ACC
                container.push(IR::IterNext(iterid));

                if f.is_await {
                    container.push(IR::Await);
                }

                match &f.left {
                    ForHead::Pat(p) => {
                        // assign directly to pattern
                        self.translate_assignment(container, p, None)?;
                    }
                    ForHead::VarDecl(v) => {
                        if v.decls.len() != 1 {
                            return Err(Error::new(v.span, "Invalid left-hand side in for-in loop: Must have a single binding.", ""));
                        }

                        let decl = &v.decls[0];

                        if decl.init.is_some() {
                            return Err(Error::new(
                                decl.span,
                                "for-in loop variable declaration may not have an initializer.",
                                "",
                            ));
                        }

                        // declare varables for the left hand name
                        let names = self.variable_names_from_pat(&decl.name);
                        for name in &names {
                            let re = match v.kind {
                                VarDeclKind::Const => container.declare_const(name).err(),
                                VarDeclKind::Let => container.declare_let(name).err(),
                                // var declare should be hoisted already
                                VarDeclKind::Var => None,
                            };
                            if let Some(err) = re {
                                return Err(Error::new(decl.span, err, ""));
                            }
                        }

                        // assign next value to pattern
                        self.translate_assignment(container, &decl.name, Some(v.kind))?;
                    }
                    ForHead::UsingDecl(_u) => {
                        todo!()
                    }
                };

                // execute the body
                self.translate_stmt(container, &f.body, None)?;

                container.push(IR::EndLoop);

                container.push(IR::DropIterator(iterid));

                container.close_context();
            }

            Stmt::Switch(s) => {
                self.translate_expr(container, &s.discriminant)?;
                let tempid = TempId::new();
                container.push(IR::StoreTemp(tempid));

                let mut cases = Vec::new();
                let mut has_default = false;

                // translate all the cases and store them in temp
                for case in &s.cases {
                    if let Some(t) = &case.test {
                        let tempid = TempId::new();
                        self.translate_expr(container, t)?;
                        container.push(IR::StoreTemp(tempid));

                        cases.push(tempid);
                    } else {
                        has_default = true;
                    }
                }

                if cases.len() == 0 && has_default {
                    // execute the default case immediately

                    let case = &s.cases[0];
                    debug_assert!(case.test.is_none());

                    container.new_context();
                    self.hoist_declares(container, &case.cons)?;

                    for s in &case.cons {
                        self.translate_stmt(container, s, None)?;
                    }

                    container.close_context();

                    container.push(IR::DropTemp(tempid));
                    return Ok(());
                }

                let mut i = 0;
                for case in &s.cases {
                    if case.test.is_none() {
                        continue;
                    }

                    // load the test target
                    container.push(IR::LoadTemp(tempid));

                    let test_id = cases[i];
                    container.push(IR::EqEqEq(test_id));

                    container.push(IR::IfElse);

                    container.push(IR::EndIf);

                    i += 1;
                }

                if has_default {
                    for case in &s.cases {
                        if case.test.is_none() {
                            container.new_context();
                            self.hoist_declares(container, &case.cons)?;

                            for s in &case.cons {
                                self.translate_stmt(container, s, None)?;
                            }

                            container.close_context();
                        }
                    }
                }

                for _ in 0..cases.len() {
                    container.push(IR::EndElse);
                }

                for case in &cases {
                    container.push(IR::DropTemp(*case));
                }
                container.push(IR::DropTemp(tempid));
            }
            Stmt::With(w) => return Err(Error::new(w.span, "with statment is deprecated.", "")),
        };
        return Ok(());
    }

    pub fn translate_decl(
        &mut self,
        container: &mut dyn IRContainer,
        decl: &Decl,
    ) -> Result<(), Error> {
        match decl {
            Decl::Fn(f) => {
                let (funcid, varid) = container.get_function(&f.ident.sym);
                self.translate_function(container, &f.function, funcid, None)?;

                container.push(IR::WriteVar(varid));
            }
            Decl::Class(c) => {
                let (classid, varid) = container.get_class(&c.ident.sym);
                self.translate_class(container, &c.class, classid)?;

                container.push(IR::WriteVar(varid));
            }
            Decl::Var(v) => {
                self.translate_vardecl(container, v)?;
            }
            Decl::TsInterface(_) => unreachable!(),
            Decl::TsEnum(_) => unreachable!(),
            Decl::TsModule(_) => unreachable!(),
            Decl::TsTypeAlias(_) => unreachable!(),
            Decl::Using(_) => todo!(),
        };
        return Ok(());
    }

    pub fn translate_vardecl(
        &mut self,
        container: &mut dyn IRContainer,
        v: &VarDecl,
    ) -> Result<(), Error> {
        for d in &v.decls {
            if let Some(e) = &d.init {
                self.translate_expr(container, &e)?;
            } else {
                container.push(IR::LoadUndefined);
            }

            self.translate_assignment(container, &d.name, Some(v.kind))?;
        }
        return Ok(());
    }

    pub fn translate_expr(
        &mut self,
        container: &mut dyn IRContainer,
        expr: &Expr,
    ) -> Result<(), Error> {
        match expr {
            Expr::Array(a) => {
                let array_id = TempId::new();
                container.push(IR::CreateArray {
                    size: a.elems.len(),
                });
                container.push(IR::StoreTemp(array_id));

                for elem in &a.elems {
                    if let Some(e) = elem {
                        self.translate_expr(container, &e.expr)?;

                        if e.spread.is_some() {
                            let iterid = IterId::new();
                            container.push(IR::CreateForOfIter(iterid));

                            container.push(IR::Loop { label: None });
                            container.push(IR::BreakIfIterDone(iterid));

                            container.push(IR::IterNext(iterid));
                            container.push(IR::ArrayPush { array: array_id });

                            container.push(IR::EndLoop);
                        } else {
                            container.push(IR::ArrayPush { array: array_id });
                        }
                    } else {
                        container.push(IR::LoadUndefined);
                        container.push(IR::ArrayPush { array: array_id });
                    }
                }

                container.push(IR::LoadTemp(array_id));
                container.push(IR::DropTemp(array_id));
            }
            Expr::Arrow(a) => {
                self.translate_arrow(container, a)?;
            }
            Expr::Assign(a) => {
                self.translate_assign_expr(container, a)?;
            }
            Expr::Await(a) => {
                if !container.is_async() {
                    return Err(Error::new(
                        a.span,
                        "await is only allowed in async context.",
                        "",
                    ));
                }
                self.translate_expr(container, &a.arg)?;
                container.push(IR::Await);
            }
            Expr::Bin(b) => {
                let left_id = TempId::new();

                // translate left
                self.translate_expr(container, &b.left)?;
                container.push(IR::StoreTemp(left_id));

                self.translate_expr(container, &b.right)?;

                let ir = match b.op {
                    BinaryOp::Add => IR::Add,
                    BinaryOp::BitAnd => IR::BitAnd,
                    BinaryOp::BitOr => IR::BitOr,
                    BinaryOp::BitXor => IR::BitXor,
                    BinaryOp::Div => IR::Div,
                    BinaryOp::EqEq => IR::EqEq,
                    BinaryOp::EqEqEq => IR::EqEqEq,
                    BinaryOp::Exp => IR::Exp,
                    BinaryOp::Gt => IR::Gt,
                    BinaryOp::GtEq => IR::GtEq,
                    BinaryOp::In => IR::In,
                    BinaryOp::InstanceOf => IR::InstanceOf,
                    BinaryOp::LShift => IR::LShift,
                    BinaryOp::LogicalAnd => IR::And,
                    BinaryOp::LogicalOr => IR::Or,
                    BinaryOp::Lt => IR::Lt,
                    BinaryOp::LtEq => IR::LtEq,
                    BinaryOp::Mod => IR::Mod,
                    BinaryOp::Mul => IR::Mul,
                    BinaryOp::NotEq => IR::NotEq,
                    BinaryOp::NotEqEq => IR::NotEqEq,
                    BinaryOp::NullishCoalescing => IR::Nullish,
                    BinaryOp::RShift => IR::RShift,
                    BinaryOp::Sub => IR::Sub,
                    BinaryOp::ZeroFillRShift => IR::ZeroFillRShift,
                };

                container.push(ir(left_id));
                container.push(IR::DropTemp(left_id));
            }
            Expr::Call(c) => {
                self.translate_call(container, c)?;
            }
            Expr::Class(c) => {
                self.translate_class(container, &c.class, ClassID::new())?;
            }
            Expr::Cond(c) => {
                let a = TempId::new();
                let b = TempId::new();
                self.translate_expr(container, &c.cons)?;
                container.push(IR::StoreTemp(a));
                self.translate_expr(container, &c.alt)?;
                container.push(IR::StoreTemp(b));

                self.translate_expr(container, &c.test)?;
                container.push(IR::IfElse);
                container.push(IR::LoadTemp(a));
                container.push(IR::EndIf);
                container.push(IR::LoadTemp(b));
                container.push(IR::EndElse);

                container.push(IR::DropTemp(a));
                container.push(IR::DropTemp(b));
            }
            Expr::Fn(f) => {
                self.translate_function(container, &f.function, FunctionId::new(), None)?;
            }
            Expr::Ident(i) => {
                if let Some(v) = container.read_var(&i.sym) {
                    container.push(IR::ReadVar(v.id()));
                } else {
                    if i.sym.as_ref() == "undefined" {
                        container.push(IR::LoadUndefined);
                    } else {
                        return Err(Error::new(
                            i.span,
                            format!("Cannot find identifier '{}'.", i.sym),
                            "",
                        ));
                    }
                }
            }
            Expr::Lit(l) => match l {
                Lit::BigInt(b) => {
                    let v = b.value.to_i64();
                    if v.is_none() {
                        return Err(Error::new(b.span, "BigInt exceeding i64 limit: -9223372036854775808 ... 9223372036854775807", "may be supported in future versions."));
                    }
                    container.push(IR::LoadBigInt(v.unwrap()))
                }
                Lit::Num(n) => {
                    if (n.value as i32) as f64 == n.value {
                        container.push(IR::LoadInt(n.value as i32));
                    } else {
                        container.push(IR::LoadNumber(n.value))
                    }
                }
                Lit::Str(s) => {
                    container.push(IR::LoadString(s.value.clone()));
                }
                Lit::Regex(r) => container.push(IR::CreateRegex {
                    pattern: JsWord::from(r.exp.as_ref()),
                    flag: JsWord::from(r.flags.as_ref()),
                }),
                Lit::Bool(b) => container.push(IR::LoadBool(b.value)),
                Lit::Null(_) => container.push(IR::LoadNull),
                Lit::JSXText(j) => {
                    return Err(Error::new(j.span, "JSXText not supported", ""));
                }
            },
            Expr::Member(m) => {
                self.translate_member_expr(container, m)?;
            }
            Expr::MetaProp(m) => match m.kind {
                MetaPropKind::ImportMeta => {
                    todo!()
                }
                MetaPropKind::NewTarget => {
                    if container.is_class_function().is_none() {
                        container.push(IR::LoadUndefined)
                    }
                    todo!()
                }
            },
            Expr::New(n) => {
                let args_list = ArgListId::new();
                let mut arg_len = 0;

                container.push(IR::CreateArgList(args_list));

                if let Some(args) = &n.args {
                    for arg in args {
                        if arg.spread.is_some() {
                            return Err(Error::new(
                                arg.spread.unwrap(),
                                "spreading arguments in 'new' constructor not supported",
                                "may be supported in the future.",
                            ));
                        }
                        self.translate_expr(container, &arg.expr)?;
                        container.push(IR::PushArg(args_list));
                        arg_len += 1;
                    }
                };

                // filter the callee and mark known errors
                match n.callee.as_ref() {
                    Expr::Array(_)
                    | Expr::Arrow(_)
                    | Expr::Bin(_)
                    | Expr::Invalid(_)
                    | Expr::Lit(_)
                    | Expr::Object(_)
                    | Expr::TaggedTpl(_)
                    | Expr::Tpl(_)
                    | Expr::Unary(_)
                    | Expr::Update(_) => {
                        return Err(Error::new(
                            n.callee.span(),
                            "(intermediate value) is not a constructor.",
                            "",
                        ))
                    }
                    _ => {}
                };

                self.translate_expr(container, &n.callee)?;

                container.push(IR::New {
                    arg_len: arg_len,
                    args: args_list,
                });
            }
            Expr::Object(o) => {
                self.translate_object_lit(container, o)?;
            }
            Expr::OptChain(o) => match o.base.as_ref() {
                OptChainBase::Member(m) => {
                    self.translate_expr(container, &m.obj)?;

                    container.push(IR::If);

                    match &m.prop {
                        MemberProp::Ident(i) => {
                            container.push(IR::ReadField { key: i.sym.clone() });
                        }
                        MemberProp::PrivateName(p) => {
                            container.push(IR::ReadField {
                                key: JsWord::from(format!("#{}", p.id.sym)),
                            });
                        }
                        MemberProp::Computed(c) => {
                            let obj_id = TempId::new();
                            container.push(IR::StoreTemp(obj_id));
                            self.translate_expr(container, &c.expr)?;

                            container.push(IR::ReadComputed { obj: obj_id });
                            container.push(IR::DropTemp(obj_id));
                        }
                    }
                    container.push(IR::EndIf);
                }
                OptChainBase::Call(c) => {
                    self.translate_expr(container, &c.callee)?;

                    container.push(IR::If);

                    let mut arg_len = 0;
                    let is_var_arg = c.args.iter().any(|a| a.spread.is_some());

                    let args_list = ArgListId::new();
                    let var_arg_id = VarArgId::new();

                    if is_var_arg {
                        container.push(IR::CreateVarArgList(var_arg_id));
                    } else {
                        container.push(IR::CreateArgList(args_list));
                    }

                    for arg in &c.args {
                        self.translate_expr(container, &arg.expr)?;

                        if arg.spread.is_some() {
                            container.push(IR::PushVarArg(var_arg_id))
                        } else {
                            container.push(IR::PushArg(args_list));
                        }

                        arg_len += 1;
                    }

                    if is_var_arg {
                        container.push(IR::CallVarArgs {
                            var_arg: var_arg_id,
                        })
                    } else {
                        container.push(IR::Call {
                            arg_len: arg_len,
                            args: args_list,
                            maybe_static: None,
                        })
                    }

                    container.push(IR::EndIf);
                }
            },
            Expr::Paren(p) => {
                self.translate_expr(container, &p.expr)?;
            }
            Expr::PrivateName(_p) => todo!(),
            Expr::Seq(s) => {
                for e in &s.exprs {
                    self.translate_expr(container, e)?;
                }
            }
            Expr::SuperProp(p) => {
                if container.is_class_function().is_some() {
                    container.push(IR::ReadSuper);

                    match &p.prop {
                        SuperProp::Ident(i) => container.push(IR::ReadField { key: i.sym.clone() }),
                        SuperProp::Computed(c) => {
                            let obj_id = TempId::new();
                            container.push(IR::StoreTemp(obj_id));

                            self.translate_expr(container, &c.expr)?;

                            container.push(IR::ReadComputed { obj: obj_id });
                            container.push(IR::DropTemp(obj_id));
                        }
                    }
                } else {
                    return Err(Error::new(p.span, "'super' can only be referenced in members of derived classes or object literal expressions.", ""));
                }
            }
            Expr::This(_t) => {
                container.push(IR::ReadThis);
            }
            Expr::TaggedTpl(t) => {
                let args_list = ArgListId::new();
                let mut arg_len = 1;

                container.push(IR::CreateArgList(args_list));

                container.push(IR::CreateArray {
                    size: t.tpl.quasis.len(),
                });
                let array_id = TempId::new();
                container.push(IR::StoreTemp(array_id));

                for i in &t.tpl.quasis {
                    if let Some(c) = &i.cooked {
                        container.push(IR::LoadString(JsWord::from(c.as_ref())))
                    } else {
                        container.push(IR::LoadString(JsWord::from(i.raw.as_ref())))
                    }

                    container.push(IR::ArrayPush { array: array_id });
                }

                container.push(IR::LoadTemp(array_id));
                container.push(IR::PushArg(args_list));
                container.push(IR::DropTemp(array_id));

                for i in &t.tpl.exprs {
                    self.translate_expr(container, &i)?;
                    container.push(IR::PushArg(args_list));
                    arg_len += 1;
                }

                self.translate_expr(container, &t.tag)?;
                container.push(IR::Call {
                    arg_len: arg_len,
                    args: args_list,
                    maybe_static: None,
                });
            }
            Expr::Tpl(t) => {
                let args = ArgListId::new();
                container.push(IR::CreateArgList(args));

                let mut quasis = Vec::new();
                for q in &t.quasis {
                    quasis.push(JsWord::from(q.raw.as_ref()));
                }

                let mut i = 0;
                for e in &t.exprs {
                    self.translate_expr(container, e)?;
                    container.push(IR::PushArg(args));
                    i += 1;
                }

                container.push(IR::LoadTpl {
                    quasis: unsafe { Box::from_raw(quasis.leak()) },
                    args_len: i,
                    args: args,
                })
            }
            Expr::Unary(u) => {
                self.translate_expr(container, &u.arg)?;
                match u.op {
                    UnaryOp::Bang => {
                        container.push(IR::Bang);
                    }
                    UnaryOp::Delete => {
                        container.push(IR::Delete);
                    }
                    UnaryOp::Minus => {
                        container.push(IR::Minus);
                    }
                    UnaryOp::Plus => {
                        container.push(IR::Plus);
                    }
                    UnaryOp::Tilde => {
                        container.push(IR::Tilde);
                    }
                    UnaryOp::TypeOf => {
                        container.push(IR::TypeOf);
                    }
                    UnaryOp::Void => {
                        container.push(IR::LoadUndefined);
                    }
                }
            }
            Expr::Update(u) => {
                self.translate_expr(container, &u.arg)?;

                let old_value = TempId::new();
                container.push(IR::StoreTemp(old_value));

                match u.op {
                    UpdateOp::PlusPlus => {
                        container.push(IR::LoadInt(1));
                        container.push(IR::Add(old_value));
                    }
                    UpdateOp::MinusMinus => {
                        container.push(IR::LoadInt(1));
                        container.push(IR::Sub(old_value));
                    }
                }

                match u.arg.as_ref() {
                    Expr::Ident(i) => {
                        self.assign_acc_to_variable(container, &i.sym, None, i.span)?;
                    }
                    Expr::Member(_m) => {
                        container.push(IR::ObjAssign);
                    }
                    Expr::OptChain(_c) => {
                        container.push(IR::ObjAssign);
                    }
                    _ => unimplemented!(),
                }

                if !u.prefix {
                    container.push(IR::LoadTemp(old_value));
                }

                container.push(IR::DropTemp(old_value))
            }

            Expr::Yield(y) => {
                if !container.is_generator() {
                    return Err(Error::new(
                        y.span,
                        "yield is only allowed in generator context.",
                        "",
                    ));
                }
                if let Some(e) = &y.arg {
                    self.translate_expr(container, e)?;
                } else {
                    container.push(IR::LoadUndefined);
                }
                container.push(IR::Yield);
            }
            _ => unimplemented!(),
        }
        return Ok(());
    }

    pub fn translate_object_lit(
        &mut self,
        container: &mut dyn IRContainer,
        o: &ObjectLit,
    ) -> Result<(), Error> {
        container.push(IR::CreateObject);

        // return immediatly if no property should be assigned
        if o.props.len() == 0 {
            return Ok(());
        }

        let obj_id = TempId::new();
        container.push(IR::StoreTemp(obj_id));

        let assign_prop = |prop: &PropName, container: &mut dyn IRContainer, this: &mut Self| {
            if let Some(prop) = prop.as_computed() {
                let value_temp = TempId::new();
                let prop_temp = TempId::new();

                // push value to stack
                container.push(IR::StoreTemp(value_temp));

                this.translate_expr(container, &prop.expr)?;
                container.push(IR::StoreTemp(prop_temp));

                container.push(IR::LoadTemp(value_temp));

                container.push(IR::WriteComputed {
                    obj: obj_id,
                    propname: prop_temp,
                });

                container.push(IR::DropTemp(value_temp));
                container.push(IR::DropTemp(prop_temp));
                return Ok(());
            }

            let name = match prop {
                PropName::BigInt(b) => JsWord::from(b.value.to_string()),
                PropName::Ident(i) => i.sym.clone(),
                PropName::Str(s) => s.value.clone(),
                PropName::Num(n) => JsWord::from(n.value.to_string()),
                PropName::Computed(_) => unreachable!(),
            };

            container.push(IR::WriteField {
                object: obj_id,
                key: name.clone(),
            });

            return Ok(());
        };

        // loop through each property
        for prop in &o.props {
            match prop {
                PropOrSpread::Prop(p) => {
                    match p.as_ref() {
                        // key value assign
                        Prop::KeyValue(k) => {
                            self.translate_expr(container, &k.value)?;
                            assign_prop(&k.key, container, self)?;
                        }
                        // not valid in object literal context
                        Prop::Assign(_a) => unreachable!(),
                        // assign variable as a key and value
                        Prop::Shorthand(s) => {
                            if let Some(v) = container.read_var(&s.sym) {
                                container.push(IR::ReadVar(v.id()));
                                container.push(IR::WriteField {
                                    object: obj_id,
                                    key: s.sym.clone(),
                                });
                            } else {
                                return Err(Error::new(
                                    s.span,
                                    format!("cannot find identifier '{}'.", s.sym),
                                    "",
                                ));
                            }
                        }
                        // treat as normal property
                        Prop::Method(m) => {
                            self.translate_function(
                                container,
                                &m.function,
                                FunctionId::new(),
                                None,
                            )?;

                            assign_prop(&m.key, container, self)?;
                        }
                        Prop::Getter(g) => {
                            return Err(Error::new(g.span, "getters are not allowed.", "see "))
                        }
                        Prop::Setter(s) => {
                            return Err(Error::new(s.span, "setters are not allowed.", "see "))
                        }
                    }
                }
                // {...something, }
                PropOrSpread::Spread(s) => {
                    self.translate_expr(container, &s.expr)?;
                    let target_id = TempId::new();
                    container.push(IR::StoreTemp(target_id));

                    let iterid = IterId::new();
                    // create a key iterator
                    container.push(IR::CreateForInIter(iterid));

                    // loop throught the keys
                    container.push(IR::Loop { label: None });
                    container.push(IR::BreakIfIterDone(iterid));
                    // get the key
                    container.push(IR::IterNext(iterid));
                    let prop_id = TempId::new();
                    container.push(IR::StoreTemp(prop_id));
                    container.push(IR::ReadComputed { obj: target_id });
                    container.push(IR::WriteComputed {
                        obj: obj_id,
                        propname: prop_id,
                    });
                    container.push(IR::DropTemp(prop_id));
                    container.push(IR::EndLoop);

                    container.push(IR::DropTemp(target_id));
                }
            };
        }

        container.push(IR::LoadTemp(obj_id));
        container.push(IR::DropTemp(obj_id));

        return Ok(());
    }

    pub fn translate_member_expr(
        &mut self,
        container: &mut dyn IRContainer,
        m: &MemberExpr,
    ) -> Result<(), Error> {
        self.translate_expr(container, &m.obj)?;

        match &m.prop {
            MemberProp::Ident(i) => {
                container.push(IR::ReadField { key: i.sym.clone() });
            }
            MemberProp::PrivateName(p) => {
                container.push(IR::ReadField {
                    key: JsWord::from(format!("#{}", p.id.sym)),
                });
            }
            MemberProp::Computed(c) => {
                if let Some(lit) = c.expr.as_lit() {
                    match lit {
                        Lit::BigInt(b) => container.push(IR::ReadField {
                            key: JsWord::from(b.value.to_string() + "n"),
                        }),

                        Lit::Bool(b) => container.push(IR::ReadField {
                            key: JsWord::from(b.value.then_some("true").unwrap_or("false")),
                        }),
                        Lit::Null(_) => container.push(IR::ReadField {
                            key: JsWord::from("null"),
                        }),
                        Lit::Num(n) => {
                            if n.value as i32 as f64 == n.value && n.value >= 0.0 {
                                container.push(IR::ReadIndex {
                                    index: n.value as usize,
                                })
                            } else {
                                container.push(IR::ReadField {
                                    key: JsWord::from(n.value.to_string()),
                                })
                            }
                        }
                        Lit::Regex(r) => container.push(IR::ReadField {
                            key: JsWord::from(format!("/{}/{}", r.exp, r.flags)),
                        }),
                        Lit::Str(s) => container.push(IR::ReadField {
                            key: s.value.clone(),
                        }),
                        Lit::JSXText(_) => unimplemented!(),
                    }
                } else {
                    let obj_id = TempId::new();
                    container.push(IR::StoreTemp(obj_id));

                    self.translate_expr(container, &c.expr)?;

                    container.push(IR::ReadComputed { obj: obj_id });
                    container.push(IR::DropTemp(obj_id));
                }
            }
        };

        return Ok(());
    }
}
