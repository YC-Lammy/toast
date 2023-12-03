use crate::util::Ident;

use super::Type;

#[derive(Debug, Default, PartialEq, Eq)]
pub struct AggregateDesc<'ctx> {
    pub fields: Vec<(Ident, Type<'ctx>)>,
}

impl<'ctx> AggregateDesc<'ctx> {
    pub const fn new() -> Self {
        Self { fields: Vec::new() }
    }
    pub fn has_field(&self, name: Ident) -> bool {
        self.fields.iter().find(|f| f.0 == name).is_some()
    }
    pub fn find_index(&self, name: Ident) -> Option<usize> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, f)| f.0 == name)
            .map(|(i, _)| i)
    }
    pub fn with_field(mut self, name: Ident, ty: Type<'ctx>) -> Self {
        self.fields.push((name, ty));
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
    pub fn has_varient(&self, name: Ident) -> bool {
        self.varients.iter().find(|f| f.0 == name).is_some()
    }
    pub fn find_index(&self, varient: Ident) -> Option<usize> {
        self.varients
            .iter()
            .enumerate()
            .find(|(_, f)| f.0 == varient)
            .map(|(i, _)| i)
    }
    pub fn with_varient(mut self, name: Ident, aggregate: Option<AggregateDesc<'ctx>>) -> Self {
        self.varients.push((name, aggregate));
        return self;
    }
}

pub struct InterfaceDesc<'ctx> {
    pub fields: Vec<(Ident, Type<'ctx>)>,
}

impl<'ctx> InterfaceDesc<'ctx> {
    pub const fn new() -> Self {
        Self { fields: Vec::new() }
    }

    pub fn has_field(&self, name: Ident) -> bool {
        self.fields.iter().find(|f| f.0 == name).is_some()
    }
    pub fn find_index(&self, name: Ident) -> Option<usize> {
        self.fields
            .iter()
            .enumerate()
            .find(|(_, f)| f.0 == name)
            .map(|(i, _)| i)
    }
    pub fn with_field(mut self, name: Ident, ty: Type<'ctx>) -> Self {
        self.fields.push((name, ty));
        return self;
    }
}
