use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

use native_ts_parser::swc_core::common::Span;
use native_ts_parser::swc_core::ecma::ast::PropName;

use crate::common::{
    ClassId, FunctionId, GenericAliasId, GenericClassId, GenericId, GenericInterfaceId, VariableId,
};

use super::{Expr, FuncType, Type};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeHash(u64);

impl TypeHash {
    pub fn from(types: &[Type]) -> Self {
        let mut hasher = std::hash::DefaultHasher::default();
        types.hash(&mut hasher);
        return Self(hasher.finish());
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MaybeGenericType {
    Type(Type),
    Generic(GenericId),
    GenericClass(GenericClassId),
    GenericInterface(GenericInterfaceId),
    GenericTypeAlias(GenericAliasId),
}

/// a generic function param
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct GenericParam {
    pub id: GenericId,
    pub name: String,
    pub constrain: Option<TypeOperator>,
    pub default: Option<TypeOperator>,
}

#[derive(Debug, Clone)]
pub struct GenericClassPropertyDesc {
    pub span: Span,
    /// type of property
    pub ty: TypeOperator,
    /// is property readonly
    pub readonly: bool,
    /// initialiser of property
    pub initialiser: Option<Expr<TypeOperator>>,
}

#[derive(Debug, Clone)]
pub struct GenericClassType {
    pub type_params: Vec<GenericParam>,

    /// class may not have constructor
    pub constructor: Option<(FunctionId, FuncType)>,

    /// static properties are just global variables
    pub static_properties: HashMap<PropName, (VariableId, Type)>,
    /// static generic methods are just generic functions
    pub static_methods: HashMap<PropName, (FunctionId, GenericFuncType)>,

    /// attributes of class
    pub properties: HashMap<PropName, GenericClassPropertyDesc>,
    /// TODO:generic methods
    pub methods: HashMap<PropName, (FunctionId, GenericFuncType)>,

    pub variance: HashMap<TypeHash, ClassId>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct GenericFuncType {
    /// type params
    pub type_params: Vec<GenericParam>,
    pub this: TypeOperator,
    /// function params
    pub params: Vec<TypeOperator>,
    /// is function variable argument
    pub var_arg: bool,
    /// return type of function
    pub return_ty: TypeOperator,
}

#[derive(Debug, Clone)]
pub struct GenericInterfacePropDesc {
    /// name of the property
    pub name: PropName,
    /// property is optional
    pub optional: bool,
    /// type of the property
    pub ty: TypeOperator,
}

#[derive(Debug, Clone)]
pub struct GenericInterfaceType {
    /// type params
    pub type_params: Vec<GenericParam>,
    /// the properties of interface
    pub properties: Vec<GenericInterfacePropDesc>,
}

#[derive(Debug, Clone)]
pub struct GenericAliasType {
    /// type params
    pub type_params: Vec<GenericParam>,
    /// generic type to be solved
    pub target: TypeOperator,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeOperator {
    /// a solid type
    Type(Type),
    /// reference a generic binding
    Generic(GenericId),
    /// generic function type
    GenericFunctionType(Box<GenericFuncType>),
    /// generic class type
    GenericClass(GenericClassId),
    /// generic interface type
    GenericInterface(GenericInterfaceId),
    /// generic alias type
    GenericAlias(GenericAliasId),

    /// provide arguments to a generic type
    Instantiation {
        span: Span,
        target: Box<TypeOperator>,
        arguments: Box<[TypeOperator]>,
    },
    /// this type
    ThisType,
    /// array type
    ArrayType(Box<TypeOperator>),
    /// tuple type
    TupleType(Box<[TypeOperator]>),
    /// function type
    FunctionType {
        this: Box<TypeOperator>,
        args: Box<[TypeOperator]>,
        return_ty: Box<TypeOperator>,
    },
    Union(Box<[TypeOperator]>),
    KeyOf(Box<TypeOperator>),
    ReadOnly(Box<TypeOperator>),
    Unique(Box<TypeOperator>),

    IndexedAccess {
        span: Span,
        target: Box<TypeOperator>,
        index_span: Span,
        index: Box<TypeOperator>,
    },
    Conditional {
        span: Span,
        test: Box<TypeOperator>,
        constrain: Box<TypeOperator>,
        left: Box<TypeOperator>,
        right: Box<TypeOperator>,
    },
    Mapped {
        span: Span,
        remove_readonly: bool,
        add_readonly: bool,
        remove_optional: bool,
        add_optional: bool,

        name: GenericId,
        indexs_span: Span,
        indexs: Box<TypeOperator>,
        name_remap: Option<Box<TypeOperator>>,
        prop_ty: Box<TypeOperator>,
    },
}

impl From<Type> for TypeOperator {
    fn from(value: Type) -> Self {
        Self::Type(value)
    }
}

impl From<MaybeGenericType> for TypeOperator {
    fn from(value: MaybeGenericType) -> Self {
        match value {
            MaybeGenericType::Type(t) => TypeOperator::Type(t),
            MaybeGenericType::Generic(g) => TypeOperator::Generic(g),
            MaybeGenericType::GenericClass(c) => TypeOperator::GenericClass(c),
            MaybeGenericType::GenericInterface(i) => TypeOperator::GenericInterface(i),
            MaybeGenericType::GenericTypeAlias(a) => TypeOperator::GenericAlias(a),
        }
    }
}
