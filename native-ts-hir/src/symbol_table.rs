use std::collections::HashMap;

use crate::ast::{FuncType, Type};




pub struct SymolTable{
    pub external_functions: HashMap<String, FuncType>,

}

impl SymolTable{
    pub fn new() -> Self{
        let external_functions = HashMap::new();

        Self { 
            external_functions 
        }
    }
}