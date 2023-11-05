use std::sync::Arc;

use crate::{untyped_hir::Stmt, PropName, VarId};

/// in typed hir, all types are concrete types
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Any,
    Null,
    Undefined,
    Int,
    Number,
    BigInt,
    String,
    Regex,
    Symbol,
    Bool,

    Class(Arc<ClassType>),
    Function(Arc<FunctionType>),
    Enum(),
    Interface(),

    Array(Box<Type>),
    TypedArray,
    /// unions are basically rust enums
    Union(Arc<[Type]>),

    Promise(Box<Type>),
    Generator {
        yield_ty: Box<Type>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ClassAttribute {
    pub name: PropName,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ClassMethod {
    pub name: PropName,
    pub is_getter: bool,
    pub is_setter: bool,
    pub function: FunctionId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ClassType {
    pub name: String,

    pub extends: Option<Type>,
    pub implments: Vec<Type>,

    pub static_props: Vec<ClassAttribute>,
    pub static_methods: Vec<ClassMethod>,

    pub attributes: Vec<ClassAttribute>,
    pub methods: Vec<ClassMethod>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionType {
    pub this: Type,
    pub params: Vec<Type>,
    pub return_ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum GenericOrType {
    Generic,
    Type(Type),
}

impl Into<Type> for GenericOrType {
    fn into(self) -> Type {
        match self {
            Self::Type(t) => t,
            _ => panic!("type is generic"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionId(pub(crate) usize);

#[derive(Debug, PartialEq, PartialOrd)]
pub struct Function {
    /// the typed id
    pub id: FunctionId,
    pub is_arrow: bool,
    pub is_async: bool,
    pub is_generator: bool,
    pub ty: Arc<FunctionType>,
    pub params: Vec<VarId>,
    pub stmts: Vec<Stmt<Type, ClassType, Function>>,
}
