use crate::common::{ClassId, EnumId, InterfaceId};

use super::FuncType;

#[repr(C)]
#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Type {
    Any,
    /// not number, string, boolean, bigint, symbol, null, or undefined.
    AnyObject,
    Undefined,
    Null,
    Bool,
    Number,
    Int,
    Bigint,
    String,
    Symbol,
    Regex,
    Object(ClassId),
    Interface(InterfaceId),
    Function(Box<FuncType>),
    Enum(EnumId),
    Array(Box<Type>),
    Promise(Box<Type>),
    Map(Box<Type>, Box<Type>),
    Union(Box<[Type]>),
    Tuple(Box<[Type]>),

    Iterator(Box<Type>),
}

impl From<super::Type> for Type {
    fn from(value: super::Type) -> Self {
        match value {
            super::Type::Alias(_) | super::Type::Generic(_) => panic!(),
            _ => unsafe { core::mem::transmute(value) },
        }
    }
}
