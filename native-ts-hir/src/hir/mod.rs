pub mod expr;
pub mod format;
pub mod function;
pub mod generic;
pub mod stmts;
pub mod strict_typed;
pub mod types;
pub mod visit;

use std::{collections::HashMap, fmt::Debug, sync::Arc};

pub use expr::*;
pub use function::*;
pub use generic::*;
use parking_lot::RwLock;
pub use stmts::*;
pub use types::*;

use crate::{
    common::{
        AliasId, ClassId, EnumId, FunctionId, GenericAliasId, GenericClassId, GenericInterfaceId,
        InterfaceId, ModuleId, VariableId,
    },
    symbol_table::SymbolTable,
    PropName,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleValueExport<TY> where TY: Debug + Clone + From<Type>{
    Var(VariableId, TY),
    Function(FunctionId),
    GenericFunction(FunctionId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleTypeExport {
    Class(ClassId),
    Interface(InterfaceId),
    Enum(EnumId),
    TypeAlias(AliasId),

    GenericClass(GenericClassId),
    GenericInterface(GenericInterfaceId),
    GenericTypeAlias(GenericAliasId),
}

pub struct Module<TY = Type> where TY: Debug + Clone + From<Type>{
    /// the unique function id of the entry function
    pub main_function: FunctionId,
    /// default export
    pub default_value_export: Option<ModuleValueExport<TY>>,
    pub default_type_export: Option<ModuleTypeExport>,
    pub default_namespace_export: Option<ModuleId>,

    /// named exports
    pub value_exports: HashMap<PropName, ModuleValueExport<TY>>,
    pub type_exports: HashMap<PropName, ModuleTypeExport>,
    pub namespcae_exports: HashMap<PropName, ModuleId>,

    /// dependencies
    pub dependencies: Vec<ModuleId>,
}

pub struct Program {
    pub table: SymbolTable,
    pub entry: ModuleId,
    pub modules: Arc<RwLock<HashMap<ModuleId, Module>>>,
}
