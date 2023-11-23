use std::collections::HashMap;

use crate::ast::*;
use crate::common::{
    AliasId, ClassId, EnumId, FunctionId, GenericId, InterfaceId, ModuleId, VariableId,
};

#[derive(Clone)]
pub enum Binding {
    /// a variable, can be let, var or const
    Var {
        /// const is not writable
        writable: bool,
        /// var can be redeclared
        redeclarable: bool,
        id: VariableId,
        ty: Type,
    },
    Using {
        id: VariableId,
        ty: Type,
    },

    GenericFunction(FunctionId),
    Function(FunctionId),

    GenericClass(ClassId),
    Class(ClassId),

    GenericInterface(InterfaceId),
    Interface(InterfaceId),

    Enum(EnumId),

    GenericTypeAlias(AliasId),
    TypeAlias(AliasId),

    NameSpace(ModuleId),

    Generic(GenericId),
}

pub struct Scope {
    /// the id of function where the scope belongs to
    pub function_id: FunctionId,
    /// generic classes
    pub bindings: HashMap<String, Binding>,
}

pub struct GenericRegister<ID, BASE> {
    pub resolved: HashMap<u64, ID>,
    pub ty: BASE,
}

pub struct Context {
    pub generic_classes: HashMap<ClassId, GenericRegister<ClassId, ()>>,
    pub classes: HashMap<ClassId, ClassType>,

    pub generic_functions: HashMap<FunctionId, GenericRegister<FunctionId, GenericFunction>>,
    pub functions: HashMap<FunctionId, Function>,

    pub generic_interfaces: HashMap<InterfaceId, GenericRegister<InterfaceId, ()>>,
    pub interfaces: HashMap<InterfaceId, InterfaceType>,

    pub enums: HashMap<EnumId, EnumType>,

    pub generic_alias: HashMap<AliasId, ()>,
    pub alias: HashMap<AliasId, Type>,

    global_scope: Scope,
    scopes: Vec<Scope>,
}

impl Context {
    pub fn new_function(&mut self, id: FunctionId) {
        self.scopes.push(Scope {
            function_id: id,
            bindings: HashMap::new(),
        });

        self.functions.insert(
            id,
            Function {
                this_ty: Type::Any,
                params: Vec::new(),
                return_ty: Type::Undefined,
                variables: HashMap::new(),
                stmts: Vec::new(),
            },
        );
    }

    pub fn end_function(&mut self) {
        let poped_scope = self.scopes.pop().expect("failed to close scope");

        // check that the last scope is not owned by current function
        if let Some(scope) = self.scopes.last() {
            assert!(
                scope.function_id != poped_scope.function_id,
                "improper closing scope"
            );
        }

        let func = self
            .functions
            .get_mut(&poped_scope.function_id)
            .expect("invalid function");

        for (_name, binding) in &poped_scope.bindings {
            match binding {
                Binding::Var { id, ty: _, .. } | Binding::Using { id, ty: _ } => {
                    func.stmts.push(Stmt::DropVar(*id));
                }
                _ => {}
            }
        }
    }

    pub fn func(&mut self) -> &mut Function {
        let scope = self.scopes.last().expect("invalid scope");

        return self
            .functions
            .get_mut(&scope.function_id)
            .expect("invalid function");
    }

    pub fn scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("invalid scope")
    }

    pub fn new_scope(&mut self) {
        let func = self.scopes.last().unwrap().function_id;

        self.scopes.push(Scope {
            function_id: func,
            bindings: HashMap::new(),
        });
    }

    pub fn end_scope(&mut self) {
        let poped_scope = self.scopes.pop().expect("failed to pop scope");

        let func = self
            .functions
            .get_mut(&poped_scope.function_id)
            .expect("invalid function");

        for (_name, binding) in &poped_scope.bindings {
            match binding {
                Binding::Var { id, ty: _, .. } | Binding::Using { id, ty: _ } => {
                    func.stmts.push(Stmt::DropVar(*id));
                }
                _ => {}
            }
        }
    }

    pub fn declare(&mut self, name: &str, binding: Binding) -> bool {
        let scope = self.scopes.last_mut().expect("invalid scope");

        if let Some(bind) = scope.bindings.get(name) {
            if let Binding::Var { redeclarable, .. } = bind {
                if *redeclarable {
                    match &binding {
                        Binding::Var {
                            writable: true,
                            redeclarable: true,
                            ..
                        } => {
                            scope.bindings.insert(name.to_string(), binding);
                            return true;
                        }
                        _ => {}
                    }
                }
            };

            return false;
        }

        match &binding {
            Binding::Var { id, ty, .. } | Binding::Using { id, ty } => {
                let id = *id;
                let ty = ty.clone();

                self.func().variables.insert(
                    id,
                    VariableDesc {
                        ty: ty,
                        is_heap: false,
                        is_captured: false,
                    },
                );
            }
            _ => {}
        }

        self.scopes
            .last_mut()
            .expect("invalide scope")
            .bindings
            .insert(name.to_string(), binding);

        return true;
    }

    pub fn find(&mut self, name: &str) -> Option<&Binding> {
        let current_func_id = self.scopes.last().unwrap().function_id;

        for scope in self.scopes.iter().rev() {
            if let Some(bind) = scope.bindings.get(name) {
                match bind {
                    Binding::Var { id, .. } => {
                        // set variable to heap
                        if scope.function_id != current_func_id {
                            // get the function that owns the
                            let func = self
                                .functions
                                .get_mut(&scope.function_id)
                                .expect("invalid function");

                            // set the variable to heap
                            let desc = func.variables.get_mut(&id).expect("invalide varaibel id");
                            desc.is_heap = true;
                        }

                        return Some(bind);
                    }
                    Binding::Using { .. } => {
                        // only the owned scope can see the variable
                        if scope.function_id != current_func_id {
                            return None;
                        }
                        return Some(bind);
                    }
                    _ => return Some(bind),
                }
            }
        }

        return None;
    }
}
