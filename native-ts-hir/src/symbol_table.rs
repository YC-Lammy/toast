use std::collections::HashMap;

use crate::{
    ast::{ClassType, EnumType, FuncType, Function, InterfaceType},
    common::{ClassId, EnumId, FunctionId, InterfaceId},
};

pub struct SymbolTable {
    pub external_functions: HashMap<String, FuncType>,
    pub functions: HashMap<FunctionId, Function>,
    pub classes: HashMap<ClassId, ClassType>,
    pub interfaces: HashMap<InterfaceId, InterfaceType>,
    pub enums: HashMap<EnumId, EnumType>,
}

impl SymbolTable {}
