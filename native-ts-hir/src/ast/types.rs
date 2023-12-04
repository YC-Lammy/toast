use std::collections::HashMap;

use crate::common::{AliasId, ClassId, EnumId, FunctionId, GenericId, InterfaceId, VariableId};
use crate::{PropName, Symbol};

use super::Expr;

#[repr(C)]
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

    Iterator(Box<Type>),

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

        if other == Type::Int {
            return self.union(Type::Number);
        }

        if self == Type::Int {
            return Type::Number.union(other);
        }

        match &self {
            Type::Int => unreachable!(),
            Type::Any => return self,
            Type::AnyObject => {
                if other.is_object() {
                    return Type::AnyObject;
                } else {
                    return Type::Union(Box::new([self, other]));
                }
            }
            Type::Bigint
            | Type::Enum(_)
            | Type::Function(_)
            | Type::Array(_)
            | Type::Bool
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
            | Type::Undefined
            | Type::Iterator(_) => Type::Union(Box::new([self, other])),
            Type::Union(u) => {
                if u.contains(&other) {
                    return self;
                }

                let mut v = Vec::with_capacity(u.len() + 1);

                for ty in u.iter() {
                    v.push(ty.clone());
                    if ty == &Type::Any {
                        return Type::Any;
                    }
                }
                if let Type::Union(u) = other {
                    for ty in u.iter() {
                        if !v.contains(ty) {
                            v.push(ty.clone());
                            if ty == &Type::Any {
                                return Type::Any;
                            }
                        }
                    }
                } else {
                    v.push(other);
                }

                v.sort();

                return Type::Union(v.into_boxed_slice());
            }
        }
    }

    pub fn is_object(&self) -> bool {
        match self {
            Type::AnyObject
            | Type::Array(_)
            | Type::Function(_)
            | Type::Map(_, _)
            | Type::Object(_)
            | Type::Promise(_)
            | Type::Regex
            | Type::Tuple(_)
            | Type::Iterator(_) => true,
            Type::Union(u) => u.iter().all(Self::is_object),
            _ => false,
        }
    }

    pub fn flattened(&self) -> Type {
        match self {
            Type::Array(_) => {}
            Type::Tuple(_) => {}
            _ => {}
        }
        todo!()
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

#[derive(Clone)]
pub struct PropertyDesc {
    pub ty: Type,
    pub readonly: bool,
    pub initialiser: Option<Expr>,
}

#[derive(Default, Clone)]
pub struct ClassType {
    pub name: String,

    pub extends: Option<ClassId>,
    pub implements: Vec<InterfaceId>,

    /// class may not have constructor
    pub constructor: Option<(FunctionId, FuncType)>,

    /// static properties are just global variables
    pub static_properties: HashMap<PropName, (VariableId, Type)>,
    /// static methods are just static functions
    pub static_methods: HashMap<PropName, (FunctionId, FuncType)>,
    /// static generic methods are just generic functions
    pub static_generic_methods: HashMap<PropName, (FunctionId,)>,

    pub properties: HashMap<PropName, PropertyDesc>,
    pub methods: HashMap<PropName, (FunctionId, FuncType)>,
    /// not used
    pub generic_methods: HashMap<PropName, (FunctionId,)>,
}

#[derive(Debug)]
pub struct InterfacePropertyDesc {
    pub ty: Type,
    pub readonly: bool,
    pub optional: bool,
}

#[derive(Debug)]
pub struct InterfaceMethod {
    pub readonly: bool,
    pub optional: bool,
    pub params: Vec<Type>,
    pub return_ty: Type,
}

#[derive(Default)]
pub struct InterfaceType {
    pub name: String,

    pub extends: Vec<ClassId>,
    pub implements: Vec<InterfaceId>,

    pub properties: HashMap<PropName, InterfacePropertyDesc>,
    pub methods: HashMap<PropName, InterfaceMethod>,
}

pub struct EnumVariantDesc {
    pub name: PropName,
}

pub struct EnumType {
    pub name: String,

    pub variants: Vec<EnumVariantDesc>,
}

pub enum StructProperty {
    Method {
        params: Box<[Type]>,
        vararg: bool,
        return_ty: Type,
    },
    Property {
        readonly: bool,
        ty: Type,
    },
}

pub struct StructType {
    pub properties: HashMap<PropName, StructProperty>,
}

#[derive(Debug, Clone, PartialOrd)]
pub enum LiteralType {
    String(Box<str>),
    Number(f64),
    Symbol(Symbol),
    Bool(bool),
    Bigint(i128),
}

impl PartialEq for LiteralType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Number(n) => match other {
                Self::Number(i) => n.total_cmp(i).is_eq(),
                _ => false,
            },
            Self::String(s) => match other {
                Self::String(n) => s == n,
                _ => false,
            },
            Self::Bigint(i) => match other {
                Self::Bigint(n) => i == n,
                _ => false,
            },
            Self::Bool(b) => match other {
                Self::Bool(p) => b == p,
                _ => false,
            },
            Self::Symbol(s) => match other {
                Self::Symbol(n) => s == n,
                _ => false,
            },
        }
    }
}
impl Eq for LiteralType {}

impl Ord for LiteralType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self {
            Self::Number(n) => match other {
                Self::Number(i) => return n.total_cmp(i),
                _ => {}
            },
            _ => {}
        };

        return self.partial_cmp(other).expect("partial compare");
    }
}
