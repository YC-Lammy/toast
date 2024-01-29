use std::hash::{Hash, Hasher};

use crate::util::Ident;

use super::Type;

#[derive(Debug, Default, PartialEq, Eq, Hash)]
pub struct AggregateDesc<'ctx> {
    pub fields: Vec<(Ident, Type<'ctx>)>,
    pub(crate) hash: u64,
}

impl<'ctx> AggregateDesc<'ctx> {
    pub const fn new() -> Self {
        Self { 
            fields: Vec::new(),
            hash: 0
        }
    }
    /// return true if has field
    pub fn has_field(&self, name: Ident) -> bool {
        self.fields.iter().find(|f| f.0 == name).is_some()
    }
    /// find index of a field
    pub fn find_index(&self, name: Ident) -> Option<usize> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, f)| f.0 == name)
            .map(|(i, _)| i)
    }
    /// add a field to aggregate
    pub fn with_field(mut self, name: Ident, ty: Type<'ctx>) -> Self {
        let mut state = ahash::AHasher::default();
        state.write_u64(self.hash);
        name.hash(&mut state);
        ty.hash(&mut state);

        self.fields.push((name, ty));
        self.hash = state.finish();

        return self;
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct EnumDesc<'ctx> {
    pub varients: Vec<(Ident, Option<AggregateDesc<'ctx>>)>,
}

impl<'ctx> EnumDesc<'ctx> {
    pub const fn new() -> Self {
        Self {
            varients: Vec::new(),
        }
    }
    /// return true if have varient
    pub fn has_varient(&self, name: Ident) -> bool {
        self.varients.iter().find(|f| f.0 == name).is_some()
    }
    /// find the index of a varient
    pub fn find_index(&self, varient: Ident) -> Option<usize> {
        self.varients
            .iter()
            .enumerate()
            .find(|(_, f)| f.0 == varient)
            .map(|(i, _)| i)
    }
    /// add a varient to enum
    pub fn with_varient(mut self, name: Ident, aggregate: Option<AggregateDesc<'ctx>>) -> Self {
        self.varients.push((name, aggregate));
        return self;
    }
}

pub struct InterfaceDesc<'ctx> {
    pub fields: Vec<(Ident, Type<'ctx>)>,
    pub(crate) hash: u64,
}

impl<'ctx> InterfaceDesc<'ctx> {
    pub const fn new() -> Self {
        Self { 
            fields: Vec::new(),
            hash: 0
        }
    }
    /// return true if interface has field
    pub fn has_field(&self, name: Ident) -> bool {
        self.fields.iter().find(|f| f.0 == name).is_some()
    }
    /// return index of field
    pub fn find_index(&self, name: Ident) -> Option<usize> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, f)| f.0 == name)
            .map(|(i, _)| i)
    }
    /// add field to interface
    pub fn with_field(mut self, name: Ident, ty: Type<'ctx>) -> Self {
        // calculate hash
        let mut state = ahash::AHasher::default();
        state.write_u64(self.hash);
        name.hash(&mut state);
        ty.hash(&mut state);

        // update hash
        self.hash = state.finish();
        // push field
        self.fields.push((name, ty));
        return self;
    }
}
