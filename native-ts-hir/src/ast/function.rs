use std::collections::HashMap;

use crate::common::VariableId;

use super::{FuncType, GenericParam};
use super::{Stmt, Type};

pub struct VariableDesc {
    pub ty: Type,
    pub is_heap: bool,
}

pub struct FunctionParam {
    pub id: VariableId,
    pub ty: Type,
}

pub struct Function {
    pub is_async: bool,
    pub is_generator: bool,
    pub this_ty: Type,
    pub params: Vec<FunctionParam>,
    pub return_ty: Type,

    pub variables: HashMap<VariableId, VariableDesc>,
    pub captures: Vec<(VariableId, Type)>,
    pub stmts: Vec<Stmt>,
}

impl Function {
    pub fn ty(&self) -> FuncType {
        FuncType {
            this_ty: self.this_ty.clone(),
            params: self.params.iter().map(|p| p.ty.clone()).collect(),
            var_arg: false,
            return_ty: self.return_ty.clone(),
        }
    }
}

pub struct GenericFunction {
    pub type_params: Vec<GenericParam>,

    pub this_ty: Type,
    pub params: Vec<FunctionParam>,
    pub return_ty: Type,

    pub variables: HashMap<VariableId, VariableDesc>,
    pub stmts: Vec<Stmt>,
}
