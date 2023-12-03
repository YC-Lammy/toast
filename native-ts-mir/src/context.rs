use std::{collections::HashMap, marker::PhantomData};

use crate::{
    function::Function,
    types::{
        aggregate::{AggregateDesc, EnumDesc, InterfaceDesc},
        FunctionType,
    },
    util::{AggregateID, EnumID, FunctionID, InterfaceID},
};

pub struct Context {
    aggregates: Vec<AggregateDesc<'static>>,
    enums: Vec<EnumDesc<'static>>,
    interfaces: Vec<InterfaceDesc<'static>>,

    // caches vtable descrptions
    implements: HashMap<(AggregateID<'static>, InterfaceID<'static>), ()>,

    functions: Vec<(
        Option<String>,
        Option<Function<'static>>,
        FunctionType<'static>,
    )>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            aggregates: Vec::new(),
            enums: Vec::new(),
            interfaces: Vec::new(),

            implements: HashMap::new(),

            functions: Vec::new(),
        }
    }

    pub fn declare_aggregate<'ctx>(&'ctx mut self, desc: AggregateDesc<'ctx>) -> AggregateID {
        let id = self.aggregates.len();
        self.aggregates.push(unsafe { core::mem::transmute(desc) });
        return AggregateID {
            id: id,
            _mark: PhantomData,
        };
    }

    pub fn get_aggregate<'ctx>(&'ctx self, id: AggregateID<'ctx>) -> &AggregateDesc<'ctx> {
        &self.aggregates[id.id]
    }

    pub fn declare_interface<'ctx>(&'ctx mut self, desc: InterfaceDesc<'ctx>) -> InterfaceID {
        let id = self.interfaces.len();
        self.interfaces.push(unsafe { core::mem::transmute(desc) });
        return InterfaceID {
            id: id,
            _mark: PhantomData,
        };
    }

    pub fn get_interface<'ctx>(&'ctx self, id: InterfaceID<'ctx>) -> &InterfaceDesc<'ctx> {
        &self.interfaces[id.id]
    }

    pub fn declare_enum<'ctx>(&'ctx mut self, desc: EnumDesc<'ctx>) -> EnumID {
        let id = self.enums.len();
        self.enums.push(unsafe { core::mem::transmute(desc) });
        return EnumID {
            id: id,
            _mark: PhantomData,
        };
    }

    pub fn get_enum<'ctx>(&'ctx self, id: EnumID<'ctx>) -> &EnumDesc {
        &self.enums[id.id]
    }

    pub fn declare_function<'ctx, S: Into<String>>(
        &'ctx mut self,
        name: Option<S>,
        ty: FunctionType<'ctx>,
    ) -> FunctionID {
        let id = self.functions.len();
        self.functions.push((name.map(|f| f.into()), None, unsafe {
            core::mem::transmute(ty)
        }));
        return FunctionID {
            id: id,
            _mark: PhantomData,
        };
    }

    pub fn define_function<'ctx>(&'ctx mut self, id: FunctionID<'ctx>, func: Function<'ctx>) {
        self.functions[id.id].1 = Some(unsafe { core::mem::transmute(func) });
    }

    pub fn get_function<'ctx>(&'ctx self, id: FunctionID<'ctx>) -> Option<&Function> {
        self.functions[id.id].1.as_ref()
    }

    pub fn get_function_type<'ctx>(&'ctx self, id: FunctionID<'ctx>) -> &'ctx FunctionType<'ctx> {
        &self.functions[id.id].2
    }
}
