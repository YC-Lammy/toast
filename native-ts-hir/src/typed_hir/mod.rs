use std::sync::Arc;

use crate::{PropName, VarId, VarKind};

/// in typed hir, all types are concrete types
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    Enum,
    Interface,

    Array(Box<Type>),
    TypedArray,
    /// unions are basically rust enums
    Union(Arc<[Type]>),

    Promise(Box<Type>),
    Generator {
        yield_ty: Box<Type>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassAttribute {
    pub name: PropName,
    pub ty: Type,
    pub initialiser: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassMethod {
    pub name: PropName,
    pub is_getter: bool,
    pub is_setter: bool,
    pub function: FunctionId,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassGenericMethod {
    pub name: PropName,
    pub function: GenericFunction,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassType {
    pub name: String,

    pub extends: Option<Type>,
    pub implments: Vec<Type>,

    pub static_props: Vec<ClassAttribute>,
    pub static_methods: Vec<ClassMethod>,
    pub static_generic_methods: Vec<ClassGenericMethod>,

    pub attributes: Vec<ClassAttribute>,
    pub methods: Vec<ClassMethod>,
    pub generic_methods: Vec<ClassGenericMethod>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionType {
    pub this: Type,
    pub params: Vec<Type>,
    pub return_ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionId(pub(crate) usize);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericFunction {
    pub stmts: Vec<Stmt<GenericOrType>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function {
    /// the typed id
    pub id: FunctionId,
    pub is_arrow: bool,
    pub is_async: bool,
    pub is_generator: bool,
    pub ty: Arc<FunctionType>,
    pub params: Vec<VarId>,
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Stmt<TY = Type> {
    Empty,
    /// declaration of a function
    /// the reason why id is untyped is that this may be a generic function
    Func(Arc<Function>),
    /// a block
    Block {
        label: Option<String>,
        stmts: Vec<Stmt>,
    },
    /// a loop
    Loop {
        label: Option<String>,
        stmts: Vec<Stmt>,
    },
    /// if else
    Condition {
        test: Expr,
        test_ty: Type,
        then: Vec<Stmt>,
        else_: Vec<Stmt>,
    },
    /// variable declare
    Declare {
        kind: VarKind,
        name: String,
        id: VarId,
        ty: Type,
        init: Option<Expr>,
    },
    /// return
    Return {
        value: Expr,
        ty: Type,
    },
    /// break, label is checked and valid
    Break {
        /// must be valid
        label: Option<String>,
    },
    /// continue
    Continue {
        /// must be valid
        label: Option<String>,
    },
    /// throw
    Throw {
        value: Expr,
        ty: Type,
    },
    /// switch
    Switch {
        test: Expr,
        test_ty: Type,
        cases: Vec<SwitchCase>,
        default: Vec<Stmt>,
    },
    /// try
    Try {
        stmts: Vec<Stmt>,
        /// the varid is not declared
        catch_binding: VarId,
        catch_binding_ty: Type,
        catch: Option<Vec<Stmt>>,
        finally: Vec<Stmt>,
    },
    /// an expr stmt
    Expr {
        expr: Box<Expr>,
        ty: TY,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SwitchCase {
    test: Expr,
    stmts: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expr<TY = Type> {
    Function {
        id: FunctionId,
        ty: Arc<FunctionType>,
    },
    StaticCall {
        function: FunctionId,
        func_ty: Arc<FunctionType>,
        args: Vec<Expr>,
    },
    Cast {
        expr: Box<Self>,
        to_ty: TY,
    },
}
