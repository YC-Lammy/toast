use std::collections::{HashMap, HashSet};

use crate::{ast::*, common::VariableId};

struct VarDesc {
    is_inited: bool,
}

struct Scope {
    variables: HashSet<VariableId>,
}

pub struct Checker {
    scopes: Vec<Scope>,
}

impl Checker {
    pub fn is_initialised(&self, id: VariableId) -> bool {
        for scope in self.scopes.iter().rev() {
            if let Some(_) = scope.variables.get(&id) {
                return true;
            }
        }
        return false;
    }
}
