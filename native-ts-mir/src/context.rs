use std::{collections::HashMap, marker::PhantomData};

use crate::{
    function::Function,
    types::{
        aggregate::{AggregateDesc, EnumDesc, InterfaceDesc},
        FunctionType,
    },
    util::{AggregateID, EnumID, FunctionID, InterfaceID, ValueID},
    Type,
};

pub(crate) struct FunctionDesc<'ctx> {
    pub(crate) name: Option<String>,
    pub(crate) is_async: bool,
    pub(crate) is_generator: bool,

    pub(crate) function: Option<Function<'ctx>>,
    pub(crate) ty: FunctionType<'ctx>,
}

pub struct Context {
    aggregates: Vec<AggregateDesc<'static>>,
    enums: Vec<EnumDesc<'static>>,
    interfaces: Vec<InterfaceDesc<'static>>,

    pub(crate) functions: Vec<FunctionDesc<'static>>,
    pub(crate) map_ssa_function: HashMap<ValueID, usize>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            aggregates: Vec::new(),
            enums: Vec::new(),
            interfaces: Vec::new(),

            functions: Vec::new(),
            map_ssa_function: HashMap::new()
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
        params: &[Type<'ctx>],
        return_ty: Type<'ctx>,
    ) -> FunctionID<'ctx> {
        let id = self.functions.len();
        self.functions.push(FunctionDesc {
            name: name.map(|s| s.into()),
            is_async: false,
            is_generator: false,
            function: None,
            ty: unsafe {
                core::mem::transmute(FunctionType {
                    params: params.into(),
                    return_: return_ty,
                })
            },
        });
        return FunctionID {
            id: id,
            _mark: PhantomData,
        };
    }

    /// alias to declare function, except returning a future.
    pub fn declare_async_function<'ctx, S: Into<String>>(
        &'ctx mut self,
        name: Option<S>,
        params: &[Type<'ctx>],
        return_ty: Type<'ctx>,
    ) -> FunctionID<'ctx> {
        let id = self.functions.len();
        self.functions.push(FunctionDesc {
            name: name.map(|s| s.into()),
            is_async: true,
            is_generator: false,
            function: None,
            ty: unsafe {
                core::mem::transmute(FunctionType {
                    params: params.into(),
                    return_: Type::Future(Box::new(return_ty)),
                })
            },
        });
        return FunctionID {
            id: id,
            _mark: PhantomData,
        };
    }

    /// alias to declare function, except returning a generator.
    pub fn declare_generator_function<'ctx, S: Into<String>>(
        &'ctx mut self,
        name: Option<S>,
        params: &[Type<'ctx>],
        yield_ty: Type<'ctx>,
        resume_ty: Type<'ctx>,
        return_ty: Type<'ctx>,
    ) -> FunctionID<'ctx> {
        let id = self.functions.len();
        self.functions.push(FunctionDesc {
            name: name.map(|s| s.into()),
            is_async: false,
            is_generator: true,
            function: None,
            ty: unsafe {
                core::mem::transmute(FunctionType {
                    params: params.into(),
                    return_: Type::Generator(Box::new((yield_ty, resume_ty, return_ty))),
                })
            },
        });
        return FunctionID {
            id: id,
            _mark: PhantomData,
        };
    }

    /// alias to declare function, except returning an async generator.
    pub fn declare_async_generator_function<'ctx, S: Into<String>>(
        &'ctx mut self,
        name: Option<S>,
        params: &[Type<'ctx>],
        yield_ty: Type<'ctx>,
        resume_ty: Type<'ctx>,
        return_ty: Type<'ctx>,
    ) -> FunctionID<'ctx> {
        let id = self.functions.len();
        self.functions.push(FunctionDesc {
            name: name.map(|s| s.into()),
            is_async: true,
            is_generator: true,
            function: None,
            ty: unsafe {
                core::mem::transmute(FunctionType {
                    params: params.into(),
                    return_: Type::Generator(Box::new((
                        Type::Future(Box::new(yield_ty)),
                        resume_ty,
                        Type::Future(Box::new(return_ty)),
                    ))),
                })
            },
        });
        return FunctionID {
            id: id,
            _mark: PhantomData,
        };
    }

    pub fn define_function<'ctx>(&'ctx mut self, id: FunctionID<'ctx>, func: Function<'ctx>) {
        self.functions[id.id].function = Some(unsafe { core::mem::transmute(func) });
    }

    pub fn get_function<'ctx>(&'ctx self, id: FunctionID<'ctx>) -> Option<&'ctx Function<'ctx>> {
        self.functions[id.id].function.as_ref()
    }

    pub fn get_function_type<'ctx>(&'ctx self, id: FunctionID<'ctx>) -> &'ctx FunctionType<'ctx> {
        &self.functions[id.id].ty
    }

    pub fn create_function<'ctx>(&'ctx self, params: &[Type<'ctx>], return_ty: Type<'ctx>, is_async: bool, is_generator: bool) -> Function<'ctx>{
        Function{
            params: params.to_vec(),
            return_: return_ty,
            is_async,
            is_generator,
            blocks: Vec::new(),
            stackslots: Vec::new(),
            _mark: PhantomData
        }
    }
}