use std::collections::HashMap;

use crate::common::VariableId;

use super::{FuncType, GenericOrType, GenericParam};
use super::{Stmt, Type};

pub struct VariableDesc<T> {
    pub ty: T,
    pub is_heap: bool,
    pub is_captured: bool,
}

pub struct FunctionParam<T> {
    pub id: VariableId,
    pub ty: T,
}

pub struct Function<T = Type> {
    pub this_ty: T,
    pub params: Vec<FunctionParam<T>>,
    pub return_ty: T,

    pub variables: HashMap<VariableId, VariableDesc<T>>,
    pub stmts: Vec<Stmt>,
}

impl Function{
    pub fn ty(&self) -> FuncType{
        FuncType { this_ty: self.this_ty.clone(), params: self.params.iter().map(|p|p.ty.clone()).collect(), var_arg: false, return_ty: self.return_ty.clone() }
    }
}

pub struct GenericFunction {
    pub type_params: Vec<GenericParam>,

    pub this_ty: GenericOrType,
    pub params: Vec<FunctionParam<GenericOrType>>,
    pub return_ty: GenericOrType,

    pub variables: HashMap<VariableId, VariableDesc<GenericOrType>>,
    pub stmts: Vec<Stmt>,
}
