use native_js_common::rc::Rc;

use crate::PropName;

use super::DeepClone;
use super::GenericParam;
use super::Type;

#[derive(Debug, Clone, PartialOrd)]
pub struct FunctionType {
    pub visit_fingerprint: usize,
    pub is_definite: bool,

    pub this_ty: Type,
    pub generics: Vec<super::GenericParam>,
    pub params: Vec<Type>,
    pub return_ty: Type,
}

impl PartialEq for FunctionType {
    fn eq(&self, other: &Self) -> bool {
        self.this_ty == other.this_ty
            && self.return_ty == other.return_ty
            && self.params == other.params
    }
}

impl FunctionType {
    pub fn minimum_generics(&self) -> usize {
        let mut i = 0;
        for g in &self.generics {
            if g.default.is_some() {
                break;
            }
            i += 1;
        }
        return i;
    }
}

#[derive(Debug, Clone, Default, PartialOrd)]
pub struct ClassType {
    pub name: String,

    pub visit_fingerprint: usize,
    pub is_definite: bool,

    pub generics: Vec<GenericParam>,

    pub extends: Option<Type>,
    pub implements: Vec<Type>,

    pub attributes: Vec<Attribute>,
    pub methods: Vec<ClassMethod>,

    pub static_props: Vec<Attribute>,
    pub static_functions: Vec<ClassMethod>,
}

impl PartialEq for ClassType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.attributes == other.attributes
            && self.methods == other.methods
            && self.static_props == other.static_props
            && self.static_functions == other.static_functions
    }
}

impl ClassType {
    pub fn minimum_generics(&self) -> usize {
        let mut i = 0;
        for g in &self.generics {
            if g.default.is_some() {
                break;
            }
            i += 1;
        }
        return i;
    }
}

impl DeepClone for ClassType {
    fn deep_clone(&self) -> ClassType {
        let mut new_self = ClassType {
            name: self.name.clone(),
            visit_fingerprint: 0,
            is_definite: true,
            generics: self.generics.deep_clone(),
            extends: self.extends.as_ref().and_then(|e| Some(e.deep_clone())),
            implements: self.implements.deep_clone(),
            attributes: Vec::new(),
            methods: Vec::new(),
            static_functions: Vec::new(),
            static_props: Vec::new(),
        };

        for attr in &self.attributes {
            new_self.attributes.push(Attribute {
                name: attr.name.clone(),
                ty: attr.ty.deep_clone(),
            })
        }

        for m in &self.methods {
            new_self.methods.push(ClassMethod {
                name: m.name.clone(),
                function: m.function.deep_clone(),
            })
        }

        for f in &self.static_functions {
            new_self.static_functions.push(ClassMethod {
                name: f.name.clone(),
                function: f.function.deep_clone(),
            })
        }

        for p in &self.static_props {
            new_self.static_props.push(Attribute {
                name: p.name.clone(),
                ty: p.ty.deep_clone(),
            })
        }

        return new_self;
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Attribute {
    pub name: PropName,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct ClassMethod {
    pub name: PropName,
    pub function: super::Function,
}

#[derive(Debug, Clone, Default, PartialOrd)]
pub struct EnumType {
    pub name: String,

    pub is_definite: bool,
    pub variants: Vec<EnumVariant>,
}

impl PartialEq for EnumType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.variants == other.variants
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct EnumVariant {
    pub name: PropName,
}

#[derive(Debug, Clone, Default, PartialOrd)]
pub struct InterfaceType {
    pub name: String,

    pub visited_fingerprint: usize,
    pub is_definite: bool,

    pub extends: Vec<Type>,

    pub generics: Vec<GenericParam>,
    pub props: Vec<InterfaceProperty>,
    pub methods: Vec<InterfaceMethod>,
}

impl PartialEq for InterfaceType {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.props == other.props && self.methods == other.methods
    }
}

impl DeepClone for InterfaceType {
    fn deep_clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            visited_fingerprint: 0,
            is_definite: true,
            extends: self.extends.deep_clone(),
            generics: self.generics.deep_clone(),
            props: self.props.deep_clone(),
            methods: self.methods.deep_clone(),
        }
    }
}

impl InterfaceType {
    pub fn minimum_generics(&self) -> usize {
        let mut i = 0;
        for g in &self.generics {
            if g.default.is_some() {
                break;
            }
            i += 1;
        }
        return i;
    }

    pub fn check(&self, other: &Type) -> bool {
        debug_assert!(self.is_definite);
        debug_assert!(self.generics.is_empty());

        match other {
            Type::Class {
                span: _,
                type_args,
                class,
            } => {
                debug_assert!(type_args.is_none());

                for m in &self.methods {
                    if let Some(method) = class.methods.iter().find(|c| c.name.eq(&c.name)) {
                        if method.function.ty.as_ref() == m.ty.as_ref() {
                            continue;
                        }
                    }
                    if !m.optional {
                        return false;
                    }
                }

                for p in &self.props {
                    if let Some(prop) = class.attributes.iter().find(|a| a.name.eq(&p.name)) {
                        if prop.ty == p.ty {
                            continue;
                        }
                    }

                    if !p.optinal {
                        return false;
                    }
                }

                return true;
            }
            Type::Interface {
                span: _,
                type_args,
                interface,
            } => {
                debug_assert!(type_args.is_none());

                return interface.as_ref() == self;
            }
            _ => return self.methods.is_empty() && self.props.is_empty(),
        };
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct InterfaceProperty {
    pub name: PropName,
    pub optinal: bool,
    pub readonly: bool,
    pub ty: Type,
}

impl DeepClone for InterfaceProperty {
    fn deep_clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            optinal: self.optinal,
            readonly: self.readonly,
            ty: self.ty.deep_clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct InterfaceMethod {
    pub name: PropName,
    pub optional: bool,
    pub ty: Rc<FunctionType>,
}

impl DeepClone for InterfaceMethod {
    fn deep_clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            optional: self.optional,
            ty: Rc::new(self.ty.deep_clone()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct AliasType {
    pub name: String,

    pub is_definite: bool,
    pub generics: Vec<super::GenericParam>,
    pub base: Type,
}

impl Default for AliasType {
    fn default() -> Self {
        Self {
            name: String::new(),
            is_definite: false,
            generics: Vec::new(),
            base: Type::Any,
        }
    }
}

impl DeepClone for AliasType {
    fn deep_clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            is_definite: true,
            generics: self.generics.iter().map(|g| g.deep_clone()).collect(),
            base: self.base.deep_clone(),
        }
    }
}

impl AliasType {
    pub fn minimum_generics(&self) -> usize {
        let mut i = 0;
        for g in &self.generics {
            if g.default.is_some() {
                break;
            }
            i += 1;
        }
        return i;
    }
}
