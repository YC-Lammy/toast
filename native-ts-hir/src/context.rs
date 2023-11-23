use native_js_common::rc::Rc;
use std::collections::HashMap;

use native_ts_parser::ModuleId;

use crate::untyped_hir::*;
use crate::VarId;
struct LabelDesc {
    name: String,
    is_loop: bool,
}

#[derive(Debug, Clone)]
pub enum Binding {
    Generic(GenericId),
    Class(Rc<ClassType>),
    Function(Rc<Function>),
    Interface(Rc<InterfaceType>),
    Enum(Rc<EnumType>),
    TypeAlias(Rc<AliasType>),
    NameSpace(ModuleId),
    Const { id: VarId, ty: Type },
    Let { id: VarId, ty: Type },
    Var { id: VarId, ty: Type },
    AwaitUsing { id: VarId, ty: Type },
    Using { id: VarId, ty: Type },
}

impl Binding {
    pub fn as_enum(&self) -> Option<&Rc<EnumType>> {
        match self {
            Self::Enum(e) => Some(e),
            _ => None,
        }
    }
    pub fn as_interface(&self) -> Option<&Rc<InterfaceType>> {
        if let Self::Interface(i) = self {
            return Some(i);
        }
        return None;
    }
    pub fn as_function(&self) -> Option<&Rc<Function>> {
        match self {
            Self::Function(f) => Some(f),
            _ => None,
        }
    }
    pub fn as_alias(&self) -> Option<&Rc<AliasType>> {
        match self {
            Self::TypeAlias(a) => Some(a),
            _ => None,
        }
    }
}

pub struct Scope {
    bindings: HashMap<String, Binding>,
}

pub struct Context {
    functions: Vec<crate::untyped_hir::Function>,
    scopes: Vec<Scope>,

    labels: Vec<LabelDesc>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            functions: vec![Default::default()],
            scopes: vec![Scope {
                bindings: HashMap::new(),
            }],
            labels: Vec::new(),
        }
    }

    pub fn new_function(&mut self) {
        self.functions.push(Default::default());
        self.new_scope();
    }

    pub fn end_function(&mut self) -> crate::untyped_hir::Function {
        self.close_scope();
        self.functions.pop().unwrap()
    }

    pub fn function(&mut self) -> &mut crate::untyped_hir::Function {
        self.functions.last_mut().unwrap()
    }

    /// opens a new scope.
    pub fn new_scope(&mut self) {
        self.scopes.push(Scope {
            bindings: HashMap::new(),
        })
    }
    /// closses a context, remove any variable and type definition out of scope
    pub fn close_scope(&mut self) {
        self.scopes
            .pop()
            .expect("compiler error: closing nonexisting context");
    }

    /// push a label, only valid within the scope
    pub fn push_label(&mut self, label: &str, is_loop: bool) {
        self.labels.push(LabelDesc {
            name: label.to_string(),
            is_loop,
        });
    }

    pub fn has_label(&self, label: &str, is_loop: bool) -> bool {
        for l in self.labels.iter().rev() {
            if l.name == label {
                if is_loop {
                    if l.is_loop {
                        return true;
                    }
                } else {
                    return true;
                }
            }
        }
        return false;
    }

    pub fn add_binding(&mut self, name: &str, binding: Binding) -> bool {
        let ctx = self.scopes.last_mut().unwrap();

        ctx.bindings.insert(name.to_string(), binding).is_none()
    }

    pub fn add_namespace(&mut self, name: &str, module: ModuleId) -> bool {
        let ctx = self.scopes.last_mut().unwrap();

        ctx.bindings
            .insert(name.to_string(), Binding::NameSpace(module))
            .is_none()
    }

    /// add a generic type definition, only valid within scope
    pub fn add_generic(&mut self, name: &str, id: GenericId) -> bool {
        let ctx = self.scopes.last_mut().unwrap();

        ctx.bindings
            .insert(name.to_string(), Binding::Generic(id))
            .is_none()
    }

    pub fn add_class(&mut self, name: &str) -> bool {
        let ctx = self.scopes.last_mut().unwrap();

        ctx.bindings
            .insert(
                name.to_string(),
                Binding::Class(Rc::new(ClassType {
                    name: name.to_string(),
                    ..Default::default()
                })),
            )
            .is_none()
    }

    pub fn add_function(&mut self, name: &str) -> bool {
        let ctx = self.scopes.last_mut().unwrap();

        ctx.bindings
            .insert(
                name.to_string(),
                Binding::Function(Rc::new(Function {
                    name: name.to_string(),
                    ..Default::default()
                })),
            )
            .is_none()
    }

    pub fn add_interface(&mut self, name: &str) -> bool {
        let ctx = self.scopes.last_mut().unwrap();

        ctx.bindings
            .insert(
                name.to_string(),
                Binding::Interface(Rc::new(InterfaceType {
                    name: name.to_string(),
                    ..Default::default()
                })),
            )
            .is_none()
    }

    pub fn add_enum(&mut self, name: &str) -> bool {
        let ctx = self.scopes.last_mut().unwrap();

        ctx.bindings
            .insert(
                name.to_string(),
                Binding::Enum(Rc::new(EnumType {
                    name: name.to_string(),
                    ..Default::default()
                })),
            )
            .is_none()
    }

    pub fn add_alias(&mut self, name: &str) -> bool {
        let ctx = self.scopes.last_mut().unwrap();

        ctx.bindings
            .insert(
                name.to_string(),
                Binding::TypeAlias(Rc::new(AliasType {
                    name: name.to_string(),
                    ..Default::default()
                })),
            )
            .is_none()
    }

    pub fn add_const(&mut self, name: &str, ty: Type) -> Option<VarId> {
        let ctx = self.scopes.last_mut().unwrap();
        let id = VarId::new();

        let err = ctx
            .bindings
            .insert(name.to_string(), Binding::Const { id, ty })
            .is_some();

        if err {
            return None;
        }
        return Some(id);
    }

    pub fn add_let(&mut self, name: &str, ty: Type) -> Option<VarId> {
        let ctx = self.scopes.last_mut().unwrap();

        let id = VarId::new();

        let err = ctx
            .bindings
            .insert(name.to_string(), Binding::Let { id, ty })
            .is_some();

        if err {
            return None;
        }
        return Some(id);
    }

    pub fn add_var(&mut self, name: &str, ty: Type) -> Option<VarId> {
        let ctx = self.scopes.last_mut().unwrap();
        let id = VarId::new();

        let err = ctx
            .bindings
            .insert(name.to_string(), Binding::Var { id, ty })
            .is_some();

        if err {
            return None;
        }
        return Some(id);
    }

    pub fn add_using(&mut self, name: &str, ty: Type) -> Option<VarId> {
        let ctx = self.scopes.last_mut().unwrap();
        let id = VarId::new();

        let err = ctx
            .bindings
            .insert(name.to_string(), Binding::Using { id, ty })
            .is_some();

        if err {
            return None;
        }
        return Some(id);
    }

    pub fn add_await_using(&mut self, name: &str, ty: Type) -> Option<VarId> {
        let ctx = self.scopes.last_mut().unwrap();
        let id = VarId::new();

        let err = ctx
            .bindings
            .insert(name.to_string(), Binding::AwaitUsing { id, ty })
            .is_some();

        if err {
            return None;
        }
        return Some(id);
    }

    pub fn find(&mut self, name: &str) -> Option<&mut Binding> {
        for i in self.scopes.iter_mut().rev() {
            if let Some(b) = i.bindings.get_mut(name) {
                return Some(b);
            }
        }

        return None;
    }
}
