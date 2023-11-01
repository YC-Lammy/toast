use std::rc::Rc;
use std::sync::Arc;

use crate::PropName;

use super::GenericParam;
use super::Type;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct FunctionType {
    pub is_definite: bool,
    
    pub this_ty: Type,
    pub generics: Vec<super::GenericParam>,
    pub params: Vec<Type>,
    pub return_ty: Type,
}

impl FunctionType{
    pub fn minimum_generics(&self) -> usize{
        let mut i =0;
        for g in &self.generics{
            if g.default.is_some(){
                break;
            }
            i += 1;
        }
        return i
    }
}

#[derive(Debug, Default, PartialEq, PartialOrd)]
pub struct ClassType {
    pub name: String,

    pub is_definite: bool,

    pub generics: Vec<GenericParam>,

    pub attributes: Vec<Attribute>,
    pub methods: Vec<ClassMethod>,

    pub static_props: Vec<Attribute>,
    pub static_functions: Vec<ClassMethod>,
}

impl ClassType{
    pub fn minimum_generics(&self) -> usize{
        let mut i =0;
        for g in &self.generics{
            if g.default.is_some(){
                break;
            }
            i += 1;
        }
        return i
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Attribute {
    pub name: PropName,
    pub ty: Type,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct ClassMethod {
    pub name: PropName,
    pub function: Arc<super::Function>,
}

#[derive(Debug, Default, PartialEq, PartialOrd)]
pub struct EnumType {
    pub name: String,

    pub is_definite: bool,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct EnumVariant {
    pub name: PropName,
}

#[derive(Debug, Default, PartialEq, PartialOrd)]
pub struct InterfaceType {
    pub name: String,

    pub is_definite: bool,
    pub extends: Vec<Type>,
    
    pub generics: Vec<GenericParam>,
    pub props: Vec<InterfaceProperty>,
    pub methods: Vec<InterfaceMethod>,
}

impl InterfaceType{
    pub fn minimum_generics(&self) -> usize{
        let mut i =0;
        for g in &self.generics{
            if g.default.is_some(){
                break;
            }
            i += 1;
        }
        return i
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct InterfaceProperty {
    pub name: PropName,
    pub optinal: bool,
    pub readonly: bool,
    pub ty: Type,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct InterfaceMethod {
    pub name: PropName,
    pub optional: bool,
    pub ty: Rc<FunctionType>,
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

impl AliasType{
    pub fn minimum_generics(&self) -> usize{
        let mut i =0;
        for g in &self.generics{
            if g.default.is_some(){
                break;
            }
            i += 1;
        }
        return i
    }
}