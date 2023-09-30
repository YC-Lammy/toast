use std::{collections::HashMap, hash::Hash};

use super::{class::ClassID, function_builder::FunctionId, VariableId};

#[derive(Debug, Clone, Copy)]
pub enum Variable {
    Binding(VariableId),
    Var(VariableId),
    Let(VariableId),
    Const(VariableId),
    Function(VariableId, FunctionId),
    Class(VariableId, ClassID),
}

impl Variable {
    pub fn id(&self) -> VariableId {
        match self {
            Self::Class(id, _)
            | Self::Const(id)
            | Self::Function(id, _)
            | Self::Binding(id)
            | Self::Let(id)
            | Self::Var(id) => *id,
        }
    }

    pub fn is_function(&self) -> bool {
        match self {
            Self::Function(_, _) => true,
            _ => false,
        }
    }

    pub fn is_class(&self) -> bool {
        match self {
            Self::Class(_, _) => true,
            _ => false,
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            Self::Const(_) => true,
            _ => false,
        }
    }
}

pub struct Context<S, T> {
    layers: Vec<HashMap<S, T>>,
}

impl<S: Eq + Hash, T> Context<S, T> {
    pub fn new() -> Self {
        Self {
            layers: vec![HashMap::new()],
        }
    }
    pub fn get(&self, name: &S) -> Option<&T> {
        let mut last = self.layers.len();

        while last > 0 {
            last -= 1;
            let last_layer = &self.layers[last];
            if let Some(t) = last_layer.get(name) {
                return Some(t);
            }
        }
        return None;
    }

    pub fn is_top_layer(&self) -> bool {
        self.layers.len() == 1
    }

    pub fn new_layer(&mut self) {
        self.layers.push(HashMap::new());
    }

    pub fn remove_layer(&mut self) {
        self.layers.pop();
    }

    pub fn has_name_on_surface(&self, name: &S) -> bool {
        let last_layer = self.layers.last().unwrap();
        return last_layer.contains_key(name);
    }

    pub fn set(&mut self, name: S, value: T) {
        let last_layer = self.layers.last_mut().unwrap();
        last_layer.insert(name, value);
    }
}
