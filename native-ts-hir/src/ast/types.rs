use std::collections::HashMap;

use crate::common::{AliasId, ClassId, EnumId, FunctionId, GenericId, InterfaceId, VariableId};
use crate::PropName;

use super::Expr;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

    Alias(AliasId),
    Generic(GenericId),
}

impl Type {
    pub fn union(self, other: Type) -> Type {
        if self == other {
            return self;
        }
        if self == Type::Any || other == Type::Any {
            return Type::Any;
        }

        if (self == Type::Number || other == Type::Number)
            && (self == Type::Int || other == Type::Int)
        {
            return Type::Number;
        }

        match &self {
            Type::Any => return self,
            Type::AnyObject => match other {
                Type::Number
                | Type::String
                | Type::Bool
                | Type::Bigint
                | Type::Symbol
                | Type::Null
                | Type::Undefined => return Type::Union(Box::new([self, other])),
                Type::AnyObject => return Type::AnyObject,
                Type::Any => return Type::Any,
                _ => return Type::AnyObject,
            },
            Type::Bigint
            | Type::Enum(_)
            | Type::Function(_)
            | Type::Array(_)
            | Type::Bool
            | Type::Int
            | Type::Interface(_)
            | Type::Null
            | Type::Number
            | Type::Object(_)
            | Type::Promise(_)
            | Type::Regex
            | Type::String
            | Type::Symbol
            | Type::Map(_, _)
            | Type::Tuple(_)
            | Type::Alias(_)
            | Type::Generic(_)
            | Type::Undefined => Type::Union(Box::new([self, other])),
            Type::Union(u) => {
                if u.contains(&other) {
                    return self;
                }

                let mut v = Vec::with_capacity(u.len() + 1);

                for ty in u.iter() {
                    v.push(ty.clone())
                }

                v.push(other);
                v.sort();

                return Type::Union(v.into_boxed_slice());
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncType {
    pub this_ty: Type,
    pub params: Vec<Type>,
    pub var_arg: bool,
    pub return_ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParam {
    pub id: GenericId,
    pub name: String,
    pub constrain: Option<InterfaceId>,
    pub extends: Option<ClassId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericFunctionType {
    pub type_params: Vec<GenericParam>,
    pub this_ty: GenericOrType,
    pub params: GenericOrType,
    pub return_ty: GenericType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericType {
    Class(ClassId),
    Interface(InterfaceId),
    Function(Box<GenericFunctionType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenericOrType {
    Generic(GenericId),
    Type(Type),
}

pub struct PropertyDesc {
    pub ty: Type,
    pub readonly: bool,
    pub initialiser: Option<Expr>,
}

#[derive(Default)]
pub struct ClassType {
    pub name: String,

    pub extends: Option<ClassId>,
    pub implements: Vec<InterfaceId>,

    pub constructor: Option<FunctionId>,
    
    /// static properties are just global variables
    pub static_properties: HashMap<PropName, (VariableId, Type)>,
    /// static methods are just static functions
    pub static_methods: HashMap<PropName, (FunctionId, FuncType)>,
    /// static generic methods are just generic functions
    pub static_generic_methods: HashMap<PropName, (FunctionId, )>,

    pub properties: HashMap<PropName, PropertyDesc>,
    pub methods: HashMap<PropName, (FunctionId, FuncType)>,
    pub generic_methods: HashMap<PropName, (FunctionId, )>,
}

#[derive(Default)]
pub struct InterfaceType {
    pub name: String,

    pub extends: Vec<ClassId>,
    pub implements: Vec<InterfaceId>,

    pub properties: HashMap<PropName, PropertyDesc>,
}

pub struct EnumVariantDesc {
    pub name: PropName,
}

pub struct EnumType {
    pub name: String,

    pub variants: Vec<EnumVariantDesc>,
}
