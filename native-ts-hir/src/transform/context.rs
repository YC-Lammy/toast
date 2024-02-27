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
        is_await: bool,
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

    global_func: FunctionId,
    scopes: Vec<Scope>,
}

impl Context {
    pub fn new() -> Self {
        // main function id
        let function_id = FunctionId::new();

        // construct scope
        let global_scope = Scope {
            function_id,
            bindings: Default::default(),
        };

        // construct context
        let mut s = Self {
            generic_classes: Default::default(),
            classes: Default::default(),
            generic_functions: Default::default(),
            functions: Default::default(),
            generic_interfaces: Default::default(),
            interfaces: Default::default(),
            enums: Default::default(),
            generic_alias: Default::default(),
            alias: Default::default(),
            global_func: function_id,
            scopes: vec![global_scope],
        };

        // insert main function
        s.functions.insert(
            function_id,
            Function {
                this_ty: Type::Any,
                params: Vec::new(),
                return_ty: Type::Undefined,
                variables: Default::default(),
                captures: Vec::new(),
                stmts: Vec::new(),
            },
        );
        return s;
    }

    pub fn new_function(&mut self, id: FunctionId) {
        self.scopes.push(Scope {
            function_id: id,
            bindings: HashMap::new(),
        });

        if !self.functions.contains_key(&id) {
            self.functions.insert(
                id,
                Function {
                    this_ty: Type::Any,
                    params: Vec::new(),
                    return_ty: Type::Undefined,
                    variables: HashMap::new(),
                    captures: Default::default(),
                    stmts: Vec::new(),
                },
            );
        }
    }

    pub fn end_function(&mut self) -> FunctionId {
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
                Binding::Var { id, ty: _, .. } | Binding::Using { id, .. } => {
                    func.stmts.push(Stmt::DropVar(*id));
                }
                _ => {}
            }
        }

        return poped_scope.function_id;
    }

    pub fn func(&mut self) -> &mut Function {
        let scope = self.scopes.last().expect("invalid scope");

        return self
            .functions
            .get_mut(&scope.function_id)
            .expect("invalid function");
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
                Binding::Var { id, ty: _, .. } | Binding::Using { id, .. } => {
                    func.stmts.push(Stmt::DropVar(*id));
                }
                _ => {}
            }
        }
    }

    pub fn declare_global(&mut self, id: VariableId, ty: Type) {
        self.functions
            .get_mut(&self.global_func)
            .expect("invalid function")
            .variables
            .insert(
                id,
                VariableDesc {
                    ty: ty,
                    is_heap: false,
                    is_captured: false,
                },
            );
    }

    pub fn declare(&mut self, name: &str, binding: Binding) -> bool {
        let scope = self.scopes.last_mut().expect("invalid scope");

        if let Some(bind) = scope.bindings.get(name) {
            if let Binding::Var {
                redeclarable,
                id: varid,
                ..
            } = bind
            {
                if *redeclarable {
                    // it is only valid if new binding is both writable and redeclarable
                    match &binding {
                        Binding::Var {
                            writable: true,
                            redeclarable: true,
                            ..
                        } => {
                            // copy the old variable id
                            let varid = *varid;
                            // replace the current binding
                            scope.bindings.insert(name.to_string(), binding);
                            // drop the variable
                            self.func().stmts.push(Stmt::DropVar(varid));
                            return true;
                        }
                        _ => {}
                    }
                }
            };

            return false;
        }

        match &binding {
            Binding::Var { id, ty, .. } | Binding::Using { id, ty, .. } => {
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
                            // set variable to a heap variable
                            desc.is_heap = true;
                        }

                        // capture variable

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

    pub fn get_func_id(&self, name: &str) -> FunctionId {
        match self.scopes.last().unwrap().bindings.get(name) {
            Some(Binding::Function(id)) => *id,
            Some(Binding::GenericFunction(id)) => *id,
            _ => unreachable!(),
        }
    }
    pub fn get_class_id(&self, name: &str) -> ClassId {
        match self.scopes.last().unwrap().bindings.get(name) {
            Some(Binding::Class(id)) => *id,
            Some(Binding::GenericClass(id)) => *id,
            _ => unreachable!(),
        }
    }
    pub fn get_interface_id(&self, name: &str) -> InterfaceId {
        match self.scopes.last().unwrap().bindings.get(name) {
            Some(Binding::Interface(id)) => *id,
            Some(Binding::GenericInterface(id)) => *id,
            _ => unreachable!(),
        }
    }
    /*
    pub fn get_variable_id(&self, name: &str) -> VariableId{
        match self.scopes.last().unwrap().bindings.get(name){
            Some(Binding::Var{ id, ..}) => *id,
            Some(Binding::Using { id,.. }) => *id,
            _ => unreachable!()
        }
    }
    */
}
