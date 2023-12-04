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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleExport {
    Undefined,
    Var(VariableId, Type),
    Function(FunctionId),
    Class(ClassId),
    Interface(InterfaceId),
    Enum(EnumId),
    Alias(AliasId),
    NameSpace(ModuleId),
}

pub struct Module {
    pub table: SymbolTable,

    pub main_function: FunctionId,
    pub default_export: ModuleExport,
    pub exports: HashMap<PropName, ModuleExport>,
}
