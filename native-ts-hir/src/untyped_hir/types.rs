use std::{
    rc::Rc,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use super::{AliasType, ClassType, EnumType, InterfaceType, FunctionType};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnknownId(usize);

impl UnknownId {
    pub fn new() -> UnknownId {
        static IDS: AtomicUsize = AtomicUsize::new(0);
        return UnknownId(IDS.fetch_add(1, Ordering::SeqCst));
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericId(usize);

impl GenericId {
    pub fn new() -> GenericId {
        static IDS: AtomicUsize = AtomicUsize::new(0);
        return GenericId(IDS.fetch_add(1, Ordering::SeqCst));
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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
    /// a function
    Function(Rc<FunctionType>),
    TypedFunction {
        type_args: Box<[Type]>,
        func: Rc<FunctionType>,
    },
    /// a union type
    Union(Vec<Type>),
    /// an instance of class
    Class(Rc<ClassType>),
    TypedClass {
        type_args: Box<[Type]>,
        class: Rc<ClassType>,
    },
    Enum(Rc<EnumType>),
    Interface(Rc<InterfaceType>),
    TypedInterface {
        type_args: Box<[Type]>,
        interface: Rc<InterfaceType>,
    },

    /// an alias type
    Alias(Rc<AliasType>),
    TypedAlias{
        type_args: Box<[Type]>,
        alias: Rc<AliasType>
    },

    /// int indexed dynamic type
    Array(Box<Type>),
    /// string indexed dynamic type
    Map(Box<Type>),
    /// a promise type
    Promise(Box<Type>),

    // ambeguous types, must be resolved before type check happens
    /// unknown, to be resolved
    Unknown(UnknownId),

    /// contextual types

    /// a generic type within fucntion
    Generic(GenericId),
    /// this type
    This,
    /// super type within class method
    Super,
    /// the retur type
    Return,
    Awaited(Box<Type>),
    Iterator(Box<Type>),
}

impl Type {
    pub fn optional(self) -> Self {
        match self {
            Self::Undefined => self,
            Self::Union(mut u) => {
                if !u.contains(&Type::Undefined) {
                    u.push(Type::Undefined);
                }
                return Self::Union(u);
            }
            _ => {
                if self > Type::Undefined {
                    Self::Union(vec![Type::Undefined, self])
                } else {
                    Self::Union(vec![self, Type::Undefined])
                }
            }
        }
    }

    pub fn awaited(self) -> Self {
        match self {
            Self::Int
            | Self::Any
            | Self::Null
            | Self::Number
            | Self::BigInt
            | Self::Bool
            | Self::String
            | Self::Regex
            | Self::Undefined
            | Self::Symbol
            | Self::Function(_)
            | Self::TypedFunction { .. }
            | Self::Array(_)
            | Self::Class(_)
            | Self::TypedClass { .. }
            | Self::Enum(_)
            | Self::Interface(_)
            | Self::TypedInterface { .. }
            | Self::This
            | Self::Super
            | Self::Iterator(_)
            | Self::Map(_) => self,
            Self::Promise(p) => *p,
            Self::Union(u) => {
                let mut v = Vec::with_capacity(u.len());
                for i in u {
                    v.push(i.awaited())
                }
                return Self::Union(v);
            }
            Self::Generic(_)
            | Self::Return
            | Self::Alias(_)
            | Self::TypedAlias { .. }
            | Self::Unknown(_)
            | Self::Awaited(_) => Self::Awaited(Box::new(self)),
        }
    }

    pub fn union(self, other: Type) -> Self {
        if self == other {
            return self;
        }

        match self {
            Self::Union(mut u) => {
                u.push(other);
                return Self::Union(u);
            }
            _ => {
                let mut v = vec![self, other];
                return Self::Union(v);
            }
        }
    }
}

impl Type {
    pub fn is_unknown(&self) -> bool {
        match self {
            
            | Self::Any
            | Self::BigInt
            | Self::Bool
            | Self::Class(_)
            | Self::TypedClass { .. }
            | Self::Enum(_)
            | Self::Function(_)
            | Self::TypedFunction { .. }
            | Self::Generic(_)
            | Self::Int
            | Self::Interface(_)
            | Self::TypedInterface { .. }
            | Self::Null
            | Self::Number
            | Self::Regex
            | Self::String
            | Self::Super
            | Self::Symbol
            | Self::This
            | Self::Undefined
            | Self::Return => false,

            Self::Alias(alias)
            | Self::TypedAlias { alias, .. } => alias.base.is_unknown(),

            Self::Map(t)
            | Self::Array(t)
            | Self::Iterator(t)
            | Self::Awaited(t)
            | Self::Promise(t) => t.is_unknown(),
            Self::Union(u) => {
                for t in u {
                    if t.is_unknown() {
                        return true;
                    }
                }
                false
            }
            Self::Unknown(_) => true,
        }
    }
    pub fn always_true(&self) -> bool {
        match self {
            Self::Alias(_)
            | Self::TypedAlias{..}
            | Self::Any
            | Self::Awaited(_)
            | Self::BigInt
            | Self::Bool
            | Self::Generic(_)
            | Self::Int
            | Self::Null
            | Self::Number
            | Self::String
            | Self::Undefined
            | Self::Unknown(_)
            | Self::Return => false,
            Self::Array(_)
            | Self::Class(_)
            | Self::TypedClass { .. }
            | Self::Function(_)
            | Self::TypedFunction { .. }
            | Self::Enum(_)
            | Self::Interface(_)
            | Self::TypedInterface { .. }
            | Self::Map(_)
            | Self::Promise(_)
            | Self::Regex
            | Self::Iterator(_)
            | Self::Super
            | Self::This
            | Self::Symbol => true,
            Self::Union(u) => {
                for u in u {
                    if !u.always_true() {
                        return false;
                    }
                }
                return true;
            }
        }
    }

    pub fn always_false(&self) -> bool {
        match self {
            Self::Undefined | Self::Null => true,

            Self::Alias(_)
            | Self::TypedAlias{..}
            | Self::Any
            | Self::Awaited(_)
            | Self::BigInt
            | Self::Bool
            | Self::Generic(_)
            | Self::Int
            | Self::Number
            | Self::String
            | Self::Unknown(_)
            | Self::Return
            | Self::Array(_)
            | Self::Class(_)
            | Self::TypedClass { .. }
            | Self::Function(_)
            | Self::TypedFunction { .. }
            | Self::Enum(_)
            | Self::Interface(_)
            | Self::TypedInterface { .. }
            | Self::Map(_)
            | Self::Promise(_)
            | Self::Iterator(_)
            | Self::Regex
            | Self::Super
            | Self::This
            | Self::Symbol => false,
            Self::Union(u) => {
                for u in u {
                    if !u.always_false() {
                        return false;
                    }
                }
                return true;
            }
        }
    }
}

