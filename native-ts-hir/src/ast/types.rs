use std::collections::HashMap;

use crate::common::{AliasId, ClassId, EnumId, FunctionId, GenericId, InterfaceId, VariableId};
use crate::{PropName, Symbol};

use super::Expr;

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    /// any type, alias of a raw interface
    Any,
    /// not number, string, boolean, bigint, symbol, null, or undefined.
    AnyObject,
    /// undefined
    Undefined,
    /// null type
    Null,
    /// boolean
    Bool,
    /// number, f64
    Number,
    /// interger, i32
    Int,
    /// big integer, i64
    Bigint,
    /// string
    String,
    /// symbol, represented as u64
    Symbol,
    /// regular expression object
    Regex,
    /// any object type
    Object(ClassId),
    /// interface type
    Interface(InterfaceId),
    /// function type
    Function(Box<FuncType>),
    /// enum type
    Enum(EnumId),
    /// array type
    Array(Box<Type>),
    /// map type
    Map(Box<Type>, Box<Type>),
    /// union type
    Union(Box<[Type]>),
    /// tuple type
    Tuple(Box<[Type]>),
    /// a promise, returned by an async function
    Promise(Box<Type>),
    /// an iterator, alias of an interface
    Iterator(Box<Type>),

    /// an alias type, should not be present after normalisation
    Alias(AliasId),
    /// a generic type, a placeholder to be resolved
    Generic(GenericId),
}

impl Type {
    /// constructs a union with another type
    pub fn union(self, other: Type) -> Type {
        // same type, not a union
        if self == other {
            return self;
        }
        // any type does not require union
        if self == Type::Any || other == Type::Any {
            return Type::Any;
        }

        // number and integer can be converted into floating point
        if (self == Type::Number || other == Type::Number)
            && (self == Type::Int || other == Type::Int)
        {
            return Type::Number;
        }

        // unions must not contain integer
        if other == Type::Int {
            return self.union(Type::Number);
        }

        // unions must not contain integer
        if self == Type::Int {
            return Type::Number.union(other);
        }

        // any object can contain any object
        if self == Type::AnyObject && other.is_object() {
            return Type::AnyObject;
        }

        // any object can contain any object
        if other == Type::AnyObject && self.is_object() {
            return Type::AnyObject;
        }

        // recuring, append union
        if other.is_union() && !self.is_union() {
            return other.union(self);
        }

        // match each case
        match &self {
            // integers are already solved
            Type::Int => unreachable!(),
            // any is solved
            Type::Any => unreachable!(),
            // already solved
            Type::AnyObject => unreachable!(),
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
            | Type::Iterator(_) => {
                // simply return a union
                Type::Union(Box::new([self, other]))
            }
            // self is already a union
            Type::Union(u) => {
                // union already contains type
                if u.contains(&other) {
                    // retunrn the union unchanged
                    return self;
                }

                // allocate vec
                let mut v = Vec::with_capacity(u.len() + 1);

                // clone each element
                for ty in u.iter() {
                    v.push(ty.clone());

                    // if element contains any, simply return any
                    if ty == &Type::Any {
                        return Type::Any;
                    }
                }

                // the other type isalso a union
                if let Type::Union(u) = other {
                    // push element in other
                    for ty in u.iter() {
                        // only pushes if not already contained
                        if !v.contains(ty) {
                            // push element
                            v.push(ty.clone());

                            // if element is any, simply return any
                            if ty == &Type::Any {
                                return Type::Any;
                            }
                        }
                    }
                } else {
                    // other is not union, push
                    v.push(other);
                }

                // sort the vec for convinience
                v.sort();

                // box the vec
                return Type::Union(v.into_boxed_slice());
            }
        }
    }

    /// returns true if self is an object type
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

    /// returns true if self is union
    pub fn is_union(&self) -> bool {
        match self {
            Self::Union(_) => true,
            _ => false,
        }
    }

    /// returns true if self is an interface
    pub fn is_interface(&self) -> bool {
        match self {
            Self::Any | Self::AnyObject | Self::Interface(_) | Self::Iterator(_) => true,
            _ => false,
        }
    }

    /// TODO: flattening types
    pub fn flattened(&self) -> Type {
        match self {
            Type::Array(_) => {}
            Type::Tuple(_) => {}
            _ => {}
        }
        todo!()
    }
}

/// a function type
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncType {
    /// the `this` param
    pub this_ty: Type,
    /// function params
    pub params: Vec<Type>,
    /// is function variable argument
    pub var_arg: bool,
    /// return type of function
    pub return_ty: Type,
}

/// a generic function param
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GenericParam {
    pub id: GenericId,
    pub name: String,
    pub constrain: Option<InterfaceId>,
    pub extends: Option<ClassId>,
}

/// an object property descriptor
#[derive(Debug, Clone)]
pub struct PropertyDesc {
    /// type of property
    pub ty: Type,
    /// is property readonly
    pub readonly: bool,
    /// initialiser of property
    pub initialiser: Option<Expr>,
}

/// a class definition
#[derive(Debug, Default, Clone)]
pub struct ClassType {
    pub name: String,

    /// parent class extends from
    pub extends: Option<ClassId>,
    /// interfaces implemented
    pub implements: Vec<InterfaceId>,

    /// class may not have constructor
    pub constructor: Option<(FunctionId, FuncType)>,

    /// static properties are just global variables
    pub static_properties: HashMap<PropName, (VariableId, Type)>,
    /// static methods are just static functions
    pub static_methods: HashMap<PropName, (FunctionId, FuncType)>,
    /// static generic methods are just generic functions
    pub static_generic_methods: HashMap<PropName, (FunctionId,)>,

    /// attributes of class
    pub properties: HashMap<PropName, PropertyDesc>,
    /// methods of class
    pub methods: HashMap<PropName, (FunctionId, FuncType)>,
    /// TODO: generic methods
    pub generic_methods: HashMap<PropName, (FunctionId,)>,
}

/// descriptor of an interface property
#[derive(Debug, Clone)]
pub struct InterfacePropertyDesc {
    /// type
    pub ty: Type,
    /// is read only
    pub readonly: bool,
    /// is property optional
    pub optional: bool,
}

/// descriptor of an interface method
#[derive(Debug, Clone)]
pub struct InterfaceMethod {
    /// is method readonly
    pub readonly: bool,
    /// is method optional
    pub optional: bool,
    /// params of method
    pub params: Vec<Type>,
    /// return type of method
    pub return_ty: Type,
}

/// an interface definition
#[derive(Debug, Default)]
pub struct InterfaceType {
    /// name of the interface, only for debugging purpose
    pub name: String,

    /// classes that extend interface
    pub extends: Vec<ClassId>,
    /// interfaces implemented by interface
    pub implements: Vec<InterfaceId>,

    /// properies of this interface
    pub properties: HashMap<PropName, InterfacePropertyDesc>,
    /// methods of this interface
    pub methods: HashMap<PropName, InterfaceMethod>,
}

/// descriptor of an enum variant
#[derive(Debug, Clone)]
pub struct EnumVariantDesc {
    /// name of the variant
    pub name: PropName,
}

/// a enum type definition
#[derive(Debug, Clone)]
pub struct EnumType {
    /// name of the enum, for debugging purpose only
    pub name: String,
    /// variants of the enum
    pub variants: Vec<EnumVariantDesc>,
}

/// literal types
#[derive(Debug, Clone, PartialOrd)]
pub enum LiteralType {
    /// string literal
    String(Box<str>),
    /// number literal
    Number(f64),
    /// integer literal
    Int(i32),
    /// symbol literal
    Symbol(Symbol),
    /// boolean literal
    Bool(bool),
    /// bigint literal
    Bigint(i128),
}

// manual implementation of equals
impl PartialEq for LiteralType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            // use total compare for f64
            Self::Number(n) => match other {
                Self::Number(i) => n.total_cmp(i).is_eq(),
                _ => false,
            },
            Self::Int(i) => {
                if let LiteralType::Int(n) = other {
                    return i == n;
                }
                other.eq(&LiteralType::Number(*i as f64))
            }
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

// total eq should work on both order
impl Eq for LiteralType {}

// manual implementation of order
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
