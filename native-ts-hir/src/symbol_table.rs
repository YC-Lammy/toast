use std::collections::HashMap;

use crate::{
    common::{AliasId, ClassId, EnumId, FunctionId, InterfaceId},
    hir::{
        ClassType, EnumType, FuncType, Function, GenericAliasType, GenericClassType,
        GenericFunction, GenericInterfaceType, InterfaceType, Type,
    },
};

/// symbol table stores all the descriptor of a module
#[derive(Default)]
pub struct SymbolTable {
    /// external functions
    pub external_functions: HashMap<String, FuncType>,
    /// functions
    pub functions: HashMap<FunctionId, Function>,
    /// classes
    pub classes: HashMap<ClassId, ClassType>,
    /// interfaces
    pub interfaces: HashMap<InterfaceId, InterfaceType>,
    /// enums
    pub enums: HashMap<EnumId, EnumType>,

    pub generic_classes: HashMap<ClassId, GenericClassType>,
    pub generic_functions: HashMap<FunctionId, GenericFunction>,
    pub generic_interfaces: HashMap<InterfaceId, GenericInterfaceType>,
    pub generic_alias: HashMap<AliasId, GenericAliasType>,
    pub alias: HashMap<AliasId, Type>,
}
