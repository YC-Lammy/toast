use std::collections::HashMap;

use crate::{ast::*, common::VariableId};

struct VarDesc {
    is_inited: bool,
}

struct Scope {
    variables: HashMap<VariableId, VarDesc>,
}

pub struct Checker {
    scopes: Vec<Scope>,
}

impl Checker {
    pub fn find(&self, id: VariableId) -> Option<&VarDesc> {
        for scope in self.scopes.iter().rev() {
            if let Some(desc) = scope.variables.get(&id) {
                return Some(desc);
            }
        }
        return None;
    }
}
