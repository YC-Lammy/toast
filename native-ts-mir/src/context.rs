use core::marker::PhantomData;

use crate::{
    function::Function,
    types::{
        aggregate::{AggregateDesc, InterfaceDesc},
        FunctionType,
    },
    util::{AggregateID, FunctionID, InterfaceID},
    Type, backend::{Backend, ObjectFile},
};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Linkage {
    Private,
    Internal,
    AvailableExternally,
    LinkOnce,
    Weak,
    Common,
    Appending,
    ExternWeak,
    LinkOnceOdr,
    WeakOdr,
    External,
    DLLExport,
    DLLImport
}

pub(crate) struct FunctionDesc<'ctx> {
    pub(crate) name: Option<String>,
    pub(crate) is_async: bool,
    pub(crate) is_generator: Option<GeneratorDesc<'ctx>>,

    pub(crate) function: Option<Function<'ctx>>,
    pub(crate) ty: FunctionType<'ctx>,
    pub(crate) linkage: Option<Linkage>,
}

pub struct GeneratorDesc<'ctx>{
    pub resume_type: Type<'ctx>,
    pub yield_type: Type<'ctx>,
}

pub struct Context {
    pub(crate) aggregates: Vec<AggregateDesc<'static>>,
    pub(crate) interfaces: Vec<InterfaceDesc<'static>>,

    pub(crate) functions: Vec<FunctionDesc<'static>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            aggregates: Vec::new(),
            interfaces: Vec::new(),

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

    pub fn declare_function<'ctx, S: Into<String>>(
        &'ctx mut self,
        name: Option<S>,
        params: &[Type<'ctx>],
        return_ty: Type<'ctx>,
        is_async: bool,
        is_generator: Option<GeneratorDesc<'ctx>>,
        linkage: Option<Linkage>,
    ) -> FunctionID<'ctx> {
        let id = self.functions.len();

        self.functions.push(FunctionDesc {
            name: name.map(|s| s.into()),
            is_async: is_async,
            is_generator: unsafe{core::mem::transmute(is_generator)},
            function: None,
            ty: unsafe {
                core::mem::transmute(FunctionType {
                    params: params.into(),
                    return_: return_ty,
                })
            },
            linkage
        });
        return FunctionID {
            id: id,
            _mark: PhantomData,
        };
    }

    pub fn define_function<'ctx>(&'ctx mut self, id: FunctionID<'ctx>, func: Function<'ctx>) {
        if let Some(desc) = self.functions.get_mut(id.id){
            if desc.is_async != func.is_async{
                panic!("function is not async but declared as async")
            }
            if let Some(gen) = &desc.is_generator{
                if let Some(fgen) = &func.is_generator{
                    if fgen.resume_type != gen.resume_type{
                        panic!("generator resume type mismatch")
                    }
                    if fgen.yield_type != gen.yield_type{
                        panic!("generator yield type mismatch")
                    }
                } else{
                    panic!("function is not generator but declared as generator")
                }
            }
            if desc.ty.params.as_ref() != &func.params{
                panic!("function params does not match")
            }
            if desc.ty.return_ != func.return_{
                panic!("function return type mismatch")
            }
            desc.function = Some(unsafe{core::mem::transmute(func)});
        } else{
            panic!("invalid function id")
        }
    }

    pub fn get_function<'ctx>(&'ctx self, id: FunctionID<'ctx>) -> Option<&'ctx Function<'ctx>> {
        self.functions[id.id].function.as_ref()
    }

    pub fn get_function_by_name<'ctx>(&'ctx self, name: &str) -> Option<&'ctx Function<'ctx>>{
        for f in &self.functions{
            if let Some(n) = &f.name{
                if name == n{
                    return f.function.as_ref()
                }
            }
        }

        return None
    }

    pub fn get_function_type<'ctx>(&'ctx self, id: FunctionID<'ctx>) -> &'ctx FunctionType<'ctx> {
        &self.functions[id.id].ty
    }

    pub fn create_function<'ctx>(&'ctx self, params: &[Type<'ctx>], return_ty: Type<'ctx>, is_async: bool, is_generator: Option<GeneratorDesc<'ctx>>) -> Function<'ctx>{
        Function{
            params: params.to_vec(),
            return_: return_ty,
            is_async,
            is_generator,
            map_ssa_func: Vec::new(),
            blocks: Vec::new(),
            stackslots: Vec::new(),
            _mark: PhantomData
        }
    }

    pub fn compile<B: Backend>(&self, mut backend: B) -> Result<ObjectFile, String>{
        backend.compile(self)
    }
}