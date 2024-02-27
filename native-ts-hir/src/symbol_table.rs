use std::collections::HashMap;

use crate::{
    ast::{ClassType, EnumType, FuncType, Function, InterfaceType},
    common::{ClassId, EnumId, FunctionId, InterfaceId},
};

/// symbol table stores all the descriptor of a module
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
}