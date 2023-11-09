pub mod expr;
pub mod stmt;
pub mod type_def;
pub mod types;
pub mod visit;

use native_js_common::rc::Rc;

pub use expr::*;
pub use stmt::*;
use swc_common::Span;
pub use type_def::*;
pub use types::*;

use crate::VarId;

pub trait DeepClone: Clone{
    fn deep_clone(&self) -> Self;
}

impl<T:DeepClone> DeepClone for Vec<T>{
    fn deep_clone(&self) -> Self {
        let mut new_vec = Vec::with_capacity(self.len());

        for i in self{
            new_vec.push(i.deep_clone());
        };
        return new_vec
    }
}

impl<T:DeepClone> DeepClone for Box<[T]>{
    fn deep_clone(&self) -> Self {
        let mut v = Vec::with_capacity(self.len());
        for i in self.iter(){
            v.push(i.deep_clone());
        }
        v.into_boxed_slice()
    }
}

impl<T:DeepClone> DeepClone for Option<T>{
    fn deep_clone(&self) -> Self {
        match self{
            Some(v) => Some(v.deep_clone()),
            None => None
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct GenericParam {
    pub name: String,
    pub span: Span,
    pub id: types::GenericId,
    pub constrain: Option<Type>,
    pub default: Option<Type>,
}

impl DeepClone for GenericParam{
    fn deep_clone(&self) -> Self {
        Self { 
            name: self.name.clone(), 
            span: self.span,
            id: self.id, 
            constrain: self.constrain.as_ref().and_then(|c|Some(c.deep_clone())), 
            default: self.constrain.as_ref().and_then(|d|Some(d.deep_clone()))
        }
    }
}

impl FunctionType{
    pub fn deep_clone(&self) -> Self{
        let mut new_self = FunctionType{
            visit_fingerprint: 0,
            is_definite: true,
            this_ty: self.this_ty.deep_clone(),
            generics: Vec::new(),
            params: Vec::new(),
            return_ty: self.return_ty.deep_clone()
        };

        for g in &self.generics{
            new_self.generics.push(g.deep_clone());
        }

        for p in &self.params{
            new_self.params.push(p.deep_clone());
        }

        return new_self
    }
}

#[derive(Debug, Clone, PartialOrd)]
pub struct Function {
    pub name: String,

    pub visitor_fingerprint: usize,
    pub is_definite: bool,

    pub is_arrow: bool,
    pub is_async: bool,
    pub is_generator: bool,
    pub ty: Rc<FunctionType>,
    pub variables: Vec<VarId>,
    pub params: Vec<VarId>,
    pub stmts: Vec<Stmt>,
}

impl PartialEq for Function{
    fn eq(&self, other: &Self) -> bool {
        self.is_arrow == other.is_arrow
        && self.is_async == other.is_async
        && self.is_generator == other.is_generator
        && self.ty == other.ty
        && self.variables == other.variables
        && self.params == other.params
        && self.stmts == other.stmts
    }
}

impl Default for Function {
    fn default() -> Self {
        Self {
            name: "main".to_string(),

            visitor_fingerprint: 0,
            is_definite: false,

            is_arrow: false,
            is_async: false,
            is_generator: false,
            ty: Rc::new(FunctionType {
                visit_fingerprint: 0,
                is_definite: false,
                this_ty: Type::Any,
                generics: Vec::new(),
                params: Vec::new(),
                return_ty: Type::Undefined,
            }),
            variables: Vec::new(),
            params: Vec::new(),
            stmts: Vec::new(),
        }
    }
}

impl DeepClone for Function{
    fn deep_clone(&self) -> Function{
        let mut new_self = Function{
            name: self.name.clone(),
            visitor_fingerprint: 0,
            is_definite: true,
            is_arrow: self.is_arrow,
            is_async: self.is_async,
            is_generator: self.is_generator,
            ty: Rc::new(self.ty.deep_clone()),
            variables: self.variables.clone(),
            params: self.params.clone(),
            stmts: Vec::new()
        };

        for s in &self.stmts{
            new_self.stmts.push(s.deep_clone());
        };

        return new_self
    } 
}