pub mod expr;
pub mod format;
pub mod function;
pub mod stmts;
pub mod strict_typed;
pub mod types;
pub mod visit;

use std::collections::HashMap;

pub use expr::*;
pub use function::*;
pub use stmts::*;
pub use types::*;

use crate::{
    common::{AliasId, ClassId, EnumId, FunctionId, InterfaceId, ModuleId, VariableId},
    symbol_table::SymbolTable,
    PropName,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleExport {
    Undefined,
    /// a variable
    Var(VariableId, Type),
    /// a function
    Function(FunctionId),
    /// a class type
    Class(ClassId),
    /// an interface
    Interface(InterfaceId),
    /// an enum
    Enum(EnumId),
    /// a type alias
    Alias(AliasId),
    /// a namespace
    NameSpace(ModuleId),
}

pub struct Module {
    /// the unique function id of the entry function
    pub main_function: FunctionId,
    /// default export
    pub default_export: ModuleExport,
    /// named exports
    pub exports: HashMap<PropName, ModuleExport>,
    /// dependencies
    pub dependencies: Vec<ModuleId>,
}

pub struct Program {
    pub table: SymbolTable,
    pub entry: ModuleId,
    pub modules: HashMap<ModuleId, Module>,
}
