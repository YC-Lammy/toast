use std::sync::atomic::{AtomicUsize, Ordering};

use swc_common::Span;

use native_js_common::rc::Rc;

use crate::PropName;

use super::{AliasType, ClassType, DeepClone, EnumType, FunctionType, InterfaceType};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnknownId(usize);

impl UnknownId {
    pub fn new() -> UnknownId {
        static IDS: AtomicUsize = AtomicUsize::new(0);
        return UnknownId(IDS.fetch_add(1, Ordering::SeqCst));
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericId(pub(crate) usize);

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
    Function {
        type_args: Option<Box<[Type]>>,
        func: Rc<FunctionType>,
    },
    /// a union type
    Union(Vec<Type>),
    /// an instance of class
    Class {
        span: Span,
        type_args: Option<Box<[Type]>>,
        class: Rc<ClassType>,
    },
    Enum(Rc<EnumType>),
    Interface {
        span: Span,
        type_args: Option<Box<[Type]>>,
        interface: Rc<InterfaceType>,
    },

    /// an alias type
    //Alias(Rc<AliasType>),
    Alias {
        span: Span,
        type_args: Option<Box<[Type]>>,
        alias: Rc<AliasType>,
    },

    /// int indexed dynamic type
    Array(Box<Type>),
    /// string indexed dynamic type
    Map(Box<Type>),
    /// a promise type
    Promise(Box<Type>),

    // ambeguous types, must be resolved before type check happens
    /// unknown, to be resolved
    Unknown {
        span: Span,
        id: UnknownId,
    },

    /// contextual types

    /// a generic type within fucntion
    Generic(GenericId),
    /// this type
    This,
    /// super type within class method
    Super,
    /// the retur type
    Return,
    Iterator(Box<Type>),
}

impl Type {
    pub fn unknown(span: Span) -> Self {
        Self::Unknown {
            span: span,
            id: UnknownId::new(),
        }
    }

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
                let v = vec![self, other];
                return Self::Union(v);
            }
        }
    }
}

impl Type {
    pub fn is_unknown(&self) -> bool {
        match self {
            Self::Any
            | Self::BigInt
            | Self::Bool
            | Self::Class { .. }
            | Self::Enum(_)
            | Self::Function { .. }
            | Self::Generic(_)
            | Self::Int
            | Self::Interface { .. }
            | Self::Null
            | Self::Number
            | Self::Regex
            | Self::String
            | Self::Super
            | Self::Symbol
            | Self::This
            | Self::Undefined
            | Self::Return => false,

            Self::Alias { alias, .. } => alias.base.is_unknown(),

            Self::Map(t) | Self::Array(t) | Self::Iterator(t) | Self::Promise(t) => t.is_unknown(),
            Self::Union(u) => {
                for t in u {
                    if t.is_unknown() {
                        return true;
                    }
                }
                false
            }
            Self::Unknown { .. } => true,
        }
    }
    pub fn always_true(&self) -> bool {
        match self {
            Self::Alias { .. }
            | Self::Any
            | Self::BigInt
            | Self::Bool
            | Self::Generic(_)
            | Self::Int
            | Self::Null
            | Self::Number
            | Self::String
            | Self::Undefined
            | Self::Unknown { .. }
            | Self::Return => false,
            Self::Array(_)
            | Self::Class { .. }
            | Self::Function { .. }
            | Self::Enum(_)
            | Self::Interface { .. }
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

            Self::Alias { .. }
            | Self::Any
            | Self::BigInt
            | Self::Bool
            | Self::Generic(_)
            | Self::Int
            | Self::Number
            | Self::String
            | Self::Unknown { .. }
            | Self::Return
            | Self::Array(_)
            | Self::Class { .. }
            | Self::Function { .. }
            | Self::Enum(_)
            | Self::Interface { .. }
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

impl DeepClone for Type {
    fn deep_clone(&self) -> Self {
        match self {
            Self::Alias {
                span,
                type_args,
                alias,
            } => Self::Alias {
                span: *span,
                type_args: type_args.deep_clone(),
                alias: Rc::new(alias.deep_clone()),
            },
            Self::Array(elem) => Self::Array(elem.deep_clone().into()),
            Self::Class {
                span,
                type_args,
                class,
            } => Self::Class {
                span: *span,
                type_args: type_args.deep_clone(),
                class: Rc::new(class.deep_clone()),
            },
            Self::Enum(e) => Self::Enum(e.clone()),
            Self::Function { type_args, func } => Self::Function {
                type_args: type_args.deep_clone(),
                func: Rc::new(func.deep_clone()),
            },
            Self::Interface {
                span,
                type_args,
                interface,
            } => Self::Interface {
                span: *span,
                type_args: type_args.deep_clone(),
                interface: Rc::new(interface.deep_clone()),
            },
            Self::Iterator(t) => Self::Iterator(t.deep_clone().into()),
            Self::Map(t) => Self::Map(t.deep_clone().into()),
            Self::Promise(p) => Self::Promise(p.deep_clone().into()),
            Self::Union(u) => Self::Union(u.deep_clone()),
            ty => ty.clone(),
        }
    }
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Alias {
                span,
                type_args,
                alias,
            } => {
                f.write_str(&alias.name)?;
                if let Some(type_args) = type_args {
                    f.write_str("<")?;

                    for (i, t) in type_args.iter().enumerate() {
                        t.fmt(f)?;

                        if i == type_args.len() - 1 {
                            break;
                        }

                        f.write_str(",");
                    }

                    f.write_str(">")?;
                }
            }
            Self::Any => f.write_str("any")?,
            Self::Array(a) => {
                a.as_ref().fmt(f)?;
                f.write_str("[]")?;
            }
            Self::BigInt => {
                f.write_str("bigint")?;
            }
            Self::Bool => {
                f.write_str("boolean")?;
            }
            Self::Class {
                span,
                type_args,
                class,
            } => {
                f.write_str(&class.name)?;
                if let Some(type_args) = type_args {
                    f.write_str("<")?;

                    for (i, t) in type_args.iter().enumerate() {
                        t.fmt(f)?;

                        if i == type_args.len() - 1 {
                            break;
                        }

                        f.write_str(",");
                    }

                    f.write_str(">")?;
                }
            }
            Self::Enum(e) => {
                f.write_str(&e.name)?;
            }
            Self::Function { type_args, func } => {
                f.write_str("(this:");
                func.this_ty.fmt(f)?;

                for p in &func.params {
                    f.write_str(",")?;
                    p.fmt(f)?;
                }

                f.write_str(")=>")?;

                func.return_ty.fmt(f)?;
            }
            Self::Generic(g) => {
                f.write_str("unknown")?;
            }
            Self::Int => {
                f.write_str("number")?;
            }
            Self::Interface {
                span,
                type_args,
                interface,
            } => {
                f.write_str(&interface.name)?;
                if let Some(type_args) = type_args {
                    f.write_str("<")?;

                    for (i, t) in type_args.iter().enumerate() {
                        t.fmt(f)?;

                        if i == type_args.len() - 1 {
                            break;
                        }

                        f.write_str(",");
                    }

                    f.write_str(">")?;
                }
            }
            Self::Iterator(t) => {
                f.write_str("Iterator<");
                t.fmt(f)?;
                f.write_str(">")?;
            }
            Self::Map(m) => {
                f.write_str("Map")?;
            }
            Self::Null => {
                f.write_str("null")?;
            }
            Self::Number => {
                f.write_str("number")?;
            }
            Self::Promise(p) => {
                f.write_str("Promise<");
                p.fmt(f)?;
                f.write_str(">")?;
            }
            Self::Regex => {
                f.write_str("RegExp")?;
            }
            Self::Return => {
                f.write_str("unknown")?;
            }
            Self::String => {
                f.write_str("string")?;
            }
            Self::Super => {
                f.write_str("Super")?;
            }
            Self::Symbol => {
                f.write_str("symbol")?;
            }
            Self::This => {
                f.write_str("this")?;
            }
            Self::Undefined => {
                f.write_str("undefined")?;
            }
            Self::Union(u) => {
                for (i, t) in u.iter().enumerate() {
                    t.fmt(f)?;

                    if i == u.len() - 1 {
                        break;
                    }
                    f.write_str("|")?;
                }
            }
            Self::Unknown { .. } => {
                f.write_str("unknown")?;
            }
        }

        return Ok(());
    }
}

impl Type {
    pub fn has_property(&self, prop: &PropName) -> bool {
        false
    }
}
