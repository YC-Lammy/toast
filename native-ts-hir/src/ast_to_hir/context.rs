use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

use native_ts_parser::swc_core::common::{Span, DUMMY_SP};

use crate::common::{
    AliasId, ClassId, EnumId, FunctionId, GenericAliasId, GenericClassId, GenericId,
    GenericInterfaceId, InterfaceId, ModuleId, VariableId,
};
use crate::hir::*;
use crate::symbol_table::SymbolTable;
use crate::util::OR;

pub enum ClassBinding {
    Generic(GenericClassId),
    Class(ClassId),
}

pub enum TypeBinding {
    GenericInterface(GenericInterfaceId),
    Interface(InterfaceId),

    Enum(EnumId),

    GenericTypeAlias(GenericAliasId),
    TypeAlias(AliasId),

    Generic(GenericId),
}

#[derive(Clone)]
pub enum ValueBinding {
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
}

pub struct Scope {
    /// the id of function where the scope belongs to
    pub function_id: FunctionId,

    class_bindings: HashMap<String, ClassBinding>,
    type_bindings: HashMap<String, TypeBinding>,
    value_bindings: HashMap<String, ValueBinding>,
    namespace_binding: HashMap<String, ModuleId>,
}

pub struct GenericRegister<ID, BASE> {
    pub resolved: HashMap<u64, ID>,
    pub ty: BASE,
}

pub struct Context {
    pub symbol_table: SymbolTable,

    pub generic_classes: HashMap<ClassId, GenericRegister<ClassId, ()>>,
    pub generic_functions: HashMap<FunctionId, GenericRegister<FunctionId, GenericFunction>>,
    pub generic_interfaces: HashMap<InterfaceId, GenericRegister<InterfaceId, ()>>,
    pub generic_alias: HashMap<AliasId, ()>,
    pub alias: HashMap<AliasId, Type>,

    global_func: FunctionId,
    scopes: Vec<Scope>,
}

impl Deref for Context {
    type Target = SymbolTable;
    fn deref(&self) -> &Self::Target {
        &self.symbol_table
    }
}

impl DerefMut for Context {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.symbol_table
    }
}

impl Context {
    pub fn new() -> Self {
        // main function id
        let function_id = FunctionId::new();

        // construct scope
        let global_scope = Scope {
            function_id,

            class_bindings: Default::default(),
            type_bindings: Default::default(),
            value_bindings: Default::default(),
            namespace_binding: Default::default(),
        };

        // construct context
        let mut s = Self {
            symbol_table: SymbolTable::default(),
            generic_classes: Default::default(),
            generic_functions: Default::default(),
            generic_interfaces: Default::default(),
            generic_alias: Default::default(),
            alias: Default::default(),
            global_func: function_id,
            scopes: vec![global_scope],
        };

        // insert main function
        s.functions.insert(
            function_id,
            Function {
                span: DUMMY_SP,
                is_async: false,
                is_generator: false,
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

    /// returns true if function is hoisted
    pub fn open_function(
        &mut self,
        span: Span,
        id: FunctionId,
        is_async: bool,
        is_generaor: bool,
    ) -> bool {
        self.scopes.push(Scope {
            function_id: id,

            class_bindings: Default::default(),
            type_bindings: Default::default(),
            value_bindings: Default::default(),
            namespace_binding: Default::default(),
        });

        if !self.functions.contains_key(&id) {
            self.functions.insert(
                id,
                Function {
                    span: span,
                    is_async: is_async,
                    is_generator: is_generaor,
                    this_ty: Type::Any,
                    params: Vec::new(),
                    return_ty: Type::Undefined,
                    variables: HashMap::new(),
                    captures: Default::default(),
                    stmts: Vec::new(),
                },
            );

            return false;
        } else {
            return true;
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

        for (_name, binding) in &poped_scope.value_bindings {
            match binding {
                ValueBinding::Var { id, ty: _, .. } | ValueBinding::Using { id, .. } => {
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
            .symbol_table
            .functions
            .get_mut(&scope.function_id)
            .expect("invalid function");
    }

    pub fn new_scope(&mut self) {
        let func = self.scopes.last().unwrap().function_id;

        self.scopes.push(Scope {
            function_id: func,

            class_bindings: Default::default(),
            type_bindings: Default::default(),
            value_bindings: Default::default(),
            namespace_binding: Default::default(),
        });
    }

    pub fn end_scope(&mut self) {
        let poped_scope = self.scopes.pop().expect("failed to pop scope");

        let func = self
            .functions
            .get_mut(&poped_scope.function_id)
            .expect("invalid function");

        for (_name, binding) in &poped_scope.value_bindings {
            match binding {
                ValueBinding::Var { id, ty: _, .. } | ValueBinding::Using { id, .. } => {
                    func.stmts.push(Stmt::DropVar(*id));
                }
                _ => {}
            }
        }
    }

    pub fn declare_global(&mut self, id: VariableId, ty: Type) {
        self.symbol_table
            .functions
            .get_mut(&self.global_func)
            .expect("invalid function")
            .variables
            .insert(
                id,
                VariableDesc {
                    ty: ty,
                    is_heap: false,
                },
            );
    }

    pub fn bind_class(&mut self, name: &str, id: ClassId) -> bool {
        let scope = self.scopes.last_mut().expect("invalid scope");

        scope
            .class_bindings
            .insert(name.to_string(), ClassBinding::Class(id))
            .is_none()
    }

    pub fn bind_generic_class(&mut self, name: &str, id: ClassId) -> bool {
        let scope = self.scopes.last_mut().expect("invalid scope");

        scope
            .class_bindings
            .insert(name.to_string(), ClassBinding::Class(id))
            .is_none()
    }

    pub fn bind_variable(
        &mut self,
        name: &str,
        id: VariableId,
        ty: Type,
        writable: bool,
        redeclarable: bool,
    ) -> bool {
        // insert variable
        self.func().variables.insert(
            id,
            VariableDesc {
                ty: ty.clone(),
                is_heap: false,
            },
        );

        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        // cannot be mixed with a class
        if scope.class_bindings.contains_key(name) {
            return false;
        }

        // check variable redeclare
        match scope.value_bindings.get_mut(name) {
            // variable declared
            Some(ValueBinding::Var {
                id: old_id,
                redeclarable: old_redeclarable,
                ty: old_ty,
                ..
            }) => {
                if !redeclarable || !*old_redeclarable {
                    return false;
                }
                if &ty != old_ty {
                    return false;
                }

                //replace old id
                let old_id = core::mem::replace(old_id, id);

                // drop the old variable
                self.func().stmts.push(Stmt::DropVar(old_id));

                return true;
            }
            // no binding
            None => {
                return scope
                    .value_bindings
                    .insert(
                        name.to_string(),
                        ValueBinding::Var {
                            writable: writable,
                            redeclarable: redeclarable,
                            id: id,
                            ty: ty,
                        },
                    )
                    .is_none();
            }
            _ => return false,
        }
    }

    pub fn bind_using(&mut self, name: &str, id: VariableId, ty: Type, is_await: bool) -> bool {
        // insert variable
        self.func().variables.insert(
            id,
            VariableDesc {
                ty: ty.clone(),
                is_heap: false,
            },
        );

        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        // cannot be mixed with a class
        if scope.class_bindings.contains_key(name) {
            return false;
        }

        return scope
            .value_bindings
            .insert(
                name.to_string(),
                ValueBinding::Using {
                    is_await,
                    id: id,
                    ty: ty,
                },
            )
            .is_none();
    }

    pub fn bind_function(&mut self, name: &str, id: FunctionId) -> bool {
        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        // cannot be mixed with a class
        if scope.class_bindings.contains_key(name) {
            return false;
        }

        return scope
            .value_bindings
            .insert(name.to_string(), ValueBinding::Function(id))
            .is_none();
    }

    pub fn bind_generic_function(&mut self, name: &str, id: FunctionId) -> bool {
        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        // cannot be mixed with a class
        if scope.class_bindings.contains_key(name) {
            return false;
        }

        return scope
            .value_bindings
            .insert(name.to_string(), ValueBinding::GenericFunction(id))
            .is_none();
    }

    pub fn bind_interface(
        &mut self,
        name: &str,
        id: InterfaceId,
    ) -> Result<Option<InterfaceId>, ()> {
        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        // interface cannot be mixed with class
        if scope.class_bindings.contains_key(name) {
            return Err(());
        }

        match scope.type_bindings.get(name) {
            Some(TypeBinding::Interface(id)) => return Ok(Some(*id)),
            None => {}
            _ => return Err(()),
        }

        scope
            .type_bindings
            .insert(name.to_string(), TypeBinding::Interface(id));

        return Ok(None);
    }

    pub fn bind_generic_interface(&mut self, name: &str, id: GenericInterfaceId) -> bool {
        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        // interface cannot be mixed with class
        if scope.class_bindings.contains_key(name) {
            return false;
        }

        return scope
            .type_bindings
            .insert(name.to_string(), TypeBinding::GenericInterface(id))
            .is_none();
    }

    pub fn bind_enum(&mut self, name: &str, id: EnumId) -> bool {
        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        // enum cannot be mixed with class
        if scope.class_bindings.contains_key(name) {
            return false;
        }

        return scope
            .type_bindings
            .insert(name.to_string(), TypeBinding::Enum(id))
            .is_none();
    }

    pub fn bind_type_alias(&mut self, name: &str, id: AliasId) -> bool {
        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        // type cannot be merged with class
        if scope.class_bindings.contains_key(name) {
            return false;
        }

        return scope
            .type_bindings
            .insert(name.to_string(), TypeBinding::TypeAlias(id))
            .is_none();
    }

    pub fn bind_generic_type_alias(&mut self, name: &str, id: GenericAliasId) -> bool {
        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        // type cannot be merged with class
        if scope.class_bindings.contains_key(name) {
            return false;
        }

        return scope
            .type_bindings
            .insert(name.to_string(), TypeBinding::GenericTypeAlias(id))
            .is_none();
    }

    pub fn bind_generic(&mut self, name: &str, id: GenericId) -> bool {
        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        if scope.class_bindings.contains_key(name) {
            return false;
        }

        return scope
            .type_bindings
            .insert(name.to_string(), TypeBinding::Generic(id))
            .is_none();
    }

    pub fn bind_namespace(&mut self, name: &str, id: ModuleId) -> Result<Option<ModuleId>, ()> {
        // get last scope
        let scope = self.scopes.last_mut().expect("invalid scope");

        // namespace cannot be merged with value
        if scope.value_bindings.contains_key(name) {
            return Err(());
        }

        if let Some(id) = scope.namespace_binding.get(name) {
            return Ok(Some(*id));
        }
        scope.namespace_binding.insert(name.to_string(), id);

        return Ok(None);
    }

    pub fn has_binding(&self, name: &str) -> bool {
        debug_assert!(!self.scopes.is_empty());

        for scope in self.scopes.iter().rev() {
            if scope.class_bindings.contains_key(name)
                || scope.type_bindings.contains_key(name)
                || scope.value_bindings.contains_key(name)
                || scope.namespace_binding.contains_key(name)
            {
                return true;
            }
        }

        return false;
    }

    /// find a class from identifier
    pub fn find_class_binding(&self, name: &str) -> Option<&ClassBinding> {
        // scopes must not be empty
        debug_assert!(!self.scopes.is_empty());

        for scope in self.scopes.iter().rev() {
            if let Some(class) = scope.class_bindings.get(name) {
                return Some(class);
            }
        }
        return None;
    }

    /// find a type from identifier
    pub fn find_type_binding(&self, name: &str) -> Option<&TypeBinding> {
        // scopes must not be empty
        debug_assert!(!self.scopes.is_empty());

        for scope in self.scopes.iter().rev() {
            if let Some(bind) = scope.type_bindings.get(name) {
                return Some(bind);
            };
        }

        return None;
    }

    /// find value from identifier
    pub fn find_value_binding(&mut self, name: &str) -> Option<&ValueBinding> {
        // scopes must not be empty
        debug_assert!(!self.scopes.is_empty());

        let current_func_id = self.scopes.last().unwrap().function_id;

        for scope in self.scopes.iter().rev() {
            if let Some(bind) = scope.value_bindings.get(name) {
                match bind {
                    ValueBinding::Var { id, .. } => {
                        // set variable to heap
                        if scope.function_id != current_func_id {
                            // get the function that owns the
                            let func = self
                                .symbol_table
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
                    ValueBinding::Using { .. } => {
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

    /// find a namespace from identifier
    pub fn find_namespace_binding(&self, name: &str) -> Option<ModuleId> {
        // scopes must not be empty
        debug_assert!(!self.scopes.is_empty());

        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.namespace_binding.get(name) {
                return Some(*id);
            }
        }

        return None;
    }

    pub fn find_namespace_or_value_binding(
        &mut self,
        name: &str,
    ) -> Option<OR<ModuleId, &ValueBinding>> {
        // scopes must not be empty
        debug_assert!(!self.scopes.is_empty());

        let current_func_id = self.scopes.last().unwrap().function_id;

        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.namespace_binding.get(name) {
                return Some(OR::A(*id));
            }

            if let Some(bind) = scope.value_bindings.get(name) {
                match bind {
                    ValueBinding::Var { id, .. } => {
                        // set variable to heap
                        if scope.function_id != current_func_id {
                            // get the function that owns the
                            let func = self
                                .symbol_table
                                .functions
                                .get_mut(&scope.function_id)
                                .expect("invalid function");

                            // set the variable to heap
                            let desc = func.variables.get_mut(&id).expect("invalide varaibel id");
                            // set variable to a heap variable
                            desc.is_heap = true;
                        }

                        // capture variable

                        return Some(OR::B(bind));
                    }
                    ValueBinding::Using { .. } => {
                        // only the owned scope can see the variable
                        if scope.function_id != current_func_id {
                            return None;
                        }
                        return Some(OR::B(bind));
                    }
                    _ => return Some(OR::B(bind)),
                }
            }
        }

        return None;
    }

    pub fn get_func_id(&self, name: &str) -> FunctionId {
        // scopes must not be empty
        debug_assert!(!self.scopes.is_empty());

        match self.scopes.last().unwrap().value_bindings.get(name) {
            Some(ValueBinding::Function(id)) => *id,
            Some(ValueBinding::GenericFunction(id)) => *id,
            _ => unreachable!(),
        }
    }

    pub fn get_class_id(&self, name: &str) -> ClassId {
        // scopes must not be empty
        debug_assert!(!self.scopes.is_empty());

        match self.scopes.last().unwrap().class_bindings.get(name) {
            Some(ClassBinding::Class(id)) => *id,
            _ => unreachable!(),
        }
    }
    pub fn get_generic_class_id(&self, name: &str) -> GenericClassId {
        // scopes must not be empty
        debug_assert!(!self.scopes.is_empty());

        match self.scopes.last().unwrap().class_bindings.get(name) {
            Some(ClassBinding::Generic(id)) => *id,
            _ => unreachable!(),
        }
    }
    pub fn get_interface_id(&self, name: &str) -> InterfaceId {
        // scopes must not be empty
        debug_assert!(!self.scopes.is_empty());

        match self.scopes.last().unwrap().type_bindings.get(name) {
            Some(TypeBinding::Interface(id)) => *id,
            _ => unreachable!(),
        }
    }

    pub fn get_generic_interface_id(&self, name: &str) -> GenericInterfaceId {
        // scopes must not be empty
        debug_assert!(!self.scopes.is_empty());

        match self.scopes.last().unwrap().type_bindings.get(name) {
            Some(TypeBinding::GenericInterface(id)) => *id,
            _ => unreachable!(),
        }
    }

    pub fn get_variable_name_ty(&self, varid: VariableId) -> Option<(&str, &Type)> {
        // loop through scopes
        for scope in self.scopes.iter().rev() {
            // loop through value bindings
            for (name, value) in &scope.value_bindings {
                match value {
                    ValueBinding::Var { id, ty, .. } if id == &varid => return Some((&name, ty)),
                    ValueBinding::Using { id, ty, .. } if id == &varid => return Some((&name, ty)),
                    _ => {}
                }
            }
        }

        return None;
    }
}
