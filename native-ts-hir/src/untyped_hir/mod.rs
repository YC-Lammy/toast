pub mod expr;
pub mod stmt;
pub mod type_def;
pub mod types;
pub mod visit;

use std::{rc::Rc, sync::Arc};

pub use expr::*;
pub use stmt::*;
pub use type_def::*;
pub use types::*;

use crate::VarId;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct GenericParam {
    pub name: String,
    pub id: types::GenericId,
    pub constrain: Option<Type>,
    pub default: Option<Type>,
}

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Function {
    pub name: String,
    pub is_definite: bool,

    pub is_arrow: bool,
    pub is_async: bool,
    pub is_generator: bool,
    pub ty: Rc<FunctionType>,
    pub variables: Vec<VarId>,
    pub params: Vec<VarId>,
    pub stmts: Vec<Stmt>,
}

impl Default for Function {
    fn default() -> Self {
        Self {
            name: "main".to_string(),
            is_definite: false,
            
            is_arrow: false,
            is_async: false,
            is_generator: false,
            ty: Rc::new(FunctionType {
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
