use native_ts_parser::swc_core::common::{Span, DUMMY_SP};

use crate::{
    ast::visit::{visit_stmts, Visitor},
    ast::{AssignOp, Callee, Expr, Program, PropNameOrExpr},
    error::Error,
    PropName,
};

/// the class constructor Pass moves initislisers into the construct.
/// it also checks for attributes being initialised.
pub fn run(program: &mut Program) -> Result<(), Vec<Error>> {
    ClassInitialisedPass::new().run(program)?;

    return Ok(());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeBase {
    Block,
    If,
    Else,
    Switch,
    SwitchCase,
    DefaultCase,
    /// anything initialised inside loop does not affect outer scope
    Loop,
    Try,
    Catch,
}

struct Scope {
    base: ScopeBase,
    is_super_called: bool,
    initialised: Vec<PropName>,
}

struct ClassInitialisedPass {
    scopes: Vec<Scope>,
    /// the last scope poped from scopes
    last_scope: Option<Scope>,

    errors: Vec<Error>,
    class: Vec<PropName>,
    constructor_span: Span,
}

impl ClassInitialisedPass {
    pub const fn new() -> Self {
        Self {
            scopes: Vec::new(),
            last_scope: None,
            errors: Vec::new(),
            class: Vec::new(),
            constructor_span: DUMMY_SP,
        }
    }
    fn run(mut self, program: &mut Program) -> Result<(), Vec<Error>> {
        for (_, class) in &mut program.table.classes {
            // class has no constructor
            if class.constructor.is_none() {
                // loop through properties
                for (_p, desc) in &mut class.properties.iter_mut() {
                    // no initialiser for attribute
                    if desc.initialiser.is_none() {
                        self.errors
                            .push(Error::syntax_error(desc.span, "missing initailiser"))
                    }
                }
                continue;
            }

            // cnstruct root scope
            let mut root_scope = Scope {
                base: ScopeBase::Block,
                is_super_called: class.extends.is_none(),
                initialised: Vec::new(),
            };

            // clone the attribut names and set initialised attributes
            self.class = class
                .properties
                .iter()
                .map(|(p, desc)| {
                    // initialiser outside of constructor
                    if desc.initialiser.is_some() {
                        // set attribute to initialised
                        root_scope.initialised.push(p.clone());
                    }
                    p.clone()
                })
                .collect();

            // push root scope
            self.scopes.push(root_scope);
            self.last_scope = None;

            // get the constructor function id
            let (funcid, _) = class.constructor.as_ref().unwrap();
            // get the constructor function
            let fun = program
                .table
                .functions
                .get(funcid)
                .expect("invalid function");

            // visit the statements
            visit_stmts(&fun.stmts, &mut self);

            // remove root scope
            let scope = self.scopes.pop().expect("expecting scope");

            // scopes should be empty
            debug_assert!(self.scopes.is_empty());

            // check that every attribute is assigned
            for (p, desc) in class.properties.iter() {
                // the property is not initiaised
                if !scope.initialised.contains(p) {
                    self.errors.push(Error::syntax_error(desc.span, format!("property '{}' of 'this' must be initialised before returning from constructor", p)))
                }
            }

            // check that super is called
            if !scope.is_super_called {
                self.errors.push(Error::syntax_error(fun.span, "Must call super constructor in derived class before accessing 'this' or returning from derived constructor"))
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors);
        }
        return Ok(());
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("expecting scope")
    }

    fn is_super_called(&mut self) -> bool {
        for s in &self.scopes {
            if s.is_super_called {
                return true;
            }
        }

        return false;
    }

    fn is_attribute_initialised(&mut self, key: &PropName) -> bool {
        // the attribute belongs to this class
        if self.class.contains(key) {
            for s in &self.scopes {
                if s.initialised.contains(key) {
                    return true;
                }
            }
            return false;
        }

        // the attribute does not belong to this class
        return true;
    }

    fn check_all_initialised(&mut self) {
        for p in unsafe { core::mem::transmute::<_, &Vec<PropName>>(&self.class) } {
            if !self.is_attribute_initialised(p) {
                self.errors.push(Error::syntax_error(self.constructor_span, format!("property '{}' of 'this' must be initialised before returning from constructor", p)))
            }
        }
    }
}

impl Visitor for ClassInitialisedPass {
    fn on_if(&mut self) {
        let s = self.current_scope().is_super_called;

        self.scopes.push(Scope {
            base: ScopeBase::If,
            is_super_called: s,
            initialised: Vec::new(),
        });
    }
    fn on_else(&mut self) {
        let s = self.current_scope().is_super_called;

        self.scopes.push(Scope {
            base: ScopeBase::Else,
            is_super_called: s,
            initialised: Vec::new(),
        })
    }

    fn on_switch(&mut self) {
        let s = self.current_scope().is_super_called;

        self.scopes.push(Scope {
            base: ScopeBase::Switch,
            is_super_called: s,
            initialised: Vec::new(),
        })
    }

    fn on_switch_case(&mut self) {
        let s = self.current_scope().is_super_called;

        self.scopes.push(Scope {
            base: ScopeBase::SwitchCase,
            is_super_called: s,
            initialised: Vec::new(),
        })
    }
    fn on_default_case(&mut self) {
        let s = self.current_scope().is_super_called;

        self.scopes.push(Scope {
            base: ScopeBase::DefaultCase,
            is_super_called: s,
            initialised: Vec::new(),
        });
    }

    fn on_loop(&mut self) {
        let s = self.current_scope().is_super_called;

        self.scopes.push(Scope {
            base: ScopeBase::Loop,
            is_super_called: s,
            initialised: Vec::new(),
        });
    }

    fn on_try(&mut self) {
        let s = self.current_scope().is_super_called;

        self.scopes.push(Scope {
            base: ScopeBase::Try,
            is_super_called: s,
            initialised: Vec::new(),
        })
    }
    fn on_catch(&mut self) {
        let s = self.current_scope().is_super_called;

        self.scopes.push(Scope {
            base: ScopeBase::Catch,
            is_super_called: s,
            initialised: Vec::new(),
        })
    }

    fn on_return(&mut self, _e: &Expr) {
        self.check_all_initialised()
    }

    fn on_close_scope(&mut self) {
        let mut scope = self.scopes.pop().expect("expecting scope");

        if let Some(last_scope) = self.last_scope.take() {
            // get the current scope
            let current_scope = self.current_scope();

            match (last_scope.base, scope.base) {
                (ScopeBase::If, ScopeBase::Else) | (ScopeBase::Try, ScopeBase::Catch) => {
                    // the 'if' scope initialises
                    for p in last_scope.initialised {
                        // the 'else' scope initialises
                        if scope.initialised.contains(&p) {
                            // both scope initiaises,
                            if !current_scope.initialised.contains(&p) {
                                current_scope.initialised.push(p);
                            }
                        }
                    }

                    // both scope has super call
                    if last_scope.is_super_called && scope.is_super_called {
                        // set super called to true
                        current_scope.is_super_called = true;
                    }
                }
                // merge the scopes
                (ScopeBase::SwitchCase, ScopeBase::SwitchCase)
                | (ScopeBase::SwitchCase, ScopeBase::DefaultCase) => {
                    scope
                        .initialised
                        .retain(|p| last_scope.initialised.contains(p));
                    scope.is_super_called = scope.is_super_called && last_scope.is_super_called;
                }
                // every thing in default case is executed
                (ScopeBase::DefaultCase, ScopeBase::Switch) => {
                    for p in last_scope.initialised {
                        if !current_scope.initialised.contains(&p) {
                            current_scope.initialised.push(p);
                        }
                        current_scope.is_super_called = last_scope.is_super_called;
                    }

                    // the 'switch' scope should not have any statement
                    debug_assert!(scope.initialised.is_empty());
                }
                _ => {}
            }
        }

        // set last scope to the poped scope
        self.last_scope = Some(scope);
    }

    fn on_this(&mut self, span: Span) {
        if !self.is_super_called() {
            self.errors.push(Error::syntax_error(span, "Must call super constructor in derived class before accessing 'this' or returning from derived constructor"))
        }
    }

    fn on_member_assign(
        &mut self,
        assign_op: AssignOp,
        object: &Expr,
        key: &PropNameOrExpr,
        _value: &Expr,
    ) {
        if let Expr::This(span) = object {
            if let PropNameOrExpr::PropName(p) = key {
                // pure assignment
                if assign_op == AssignOp::Assign {
                    // get the current scope
                    let current_scope = self.current_scope();
                    // set propety to initialised
                    if !current_scope.initialised.contains(p) {
                        current_scope.initialised.push(p.clone());
                    }
                } else {
                    // not a pure assignment, the field is read before reassigned
                    if !self.is_attribute_initialised(p) {
                        self.errors.push(Error::syntax_error(
                            *span,
                            format!("property '{}' of 'this' must be initialised before use", p),
                        ))
                    }
                }
            }
        }
    }

    fn on_member(&mut self, object: &Expr, key: &PropNameOrExpr, _optional: bool) {
        if let Expr::This(span) = object {
            if let PropNameOrExpr::PropName(p) = key {
                // check if the attribute is initialised
                if !self.is_attribute_initialised(p) {
                    self.errors.push(Error::syntax_error(
                        *span,
                        format!("property '{}' of 'this' must be initialised before use", p),
                    ))
                }
            }
        }
    }

    fn on_member_update(
        &mut self,
        _update_op: crate::ast::UpdateOp,
        object: &Expr,
        key: &PropNameOrExpr,
    ) {
        if let Expr::This(span) = object {
            if let PropNameOrExpr::PropName(p) = key {
                // check if the attribute is initialised
                if !self.is_attribute_initialised(p) {
                    self.errors.push(Error::syntax_error(
                        *span,
                        format!("property '{}' of 'this' must be initialised before use", p),
                    ))
                }
            }
        }
    }

    fn on_call(&mut self, callee: &Callee, _args: &[Expr], optional: bool) {
        match callee {
            Callee::Super(_) => {}
            _ => return,
        }

        // super call cannot be optional
        debug_assert!(!optional);

        if self.is_super_called() {
            self.errors.push(Error::syntax_error(
                DUMMY_SP,
                "Super constructor may only be called once",
            ))
        }

        // set is super called to true
        self.current_scope().is_super_called = true;
    }
}
