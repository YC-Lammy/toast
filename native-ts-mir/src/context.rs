use core::marker::PhantomData;

use crate::{
    function::Function,
    types::{
        aggregate::{AggregateDesc, InterfaceDesc},
        FunctionType,
    },
    util::{AggregateID, FunctionID, InterfaceID},
    Type, backend::Backend,
};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Linkage {
    /// private symbol
    Private,
    /// internal
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
    /// export to dll
    DLLExport,
    /// import from dll
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

    /// define an aggregate type
    pub fn declare_aggregate<'ctx>(&mut self, desc: AggregateDesc<'ctx>) -> AggregateID {
        // check if aggregate already declared
        for (i, agg) in self.aggregates.iter().enumerate(){
            if agg.hash == desc.hash{
                return AggregateID{
                    id: i,
                    _mark: PhantomData
                }
            }
        }

        // get the id
        let id = self.aggregates.len();
        // push aggregate
        self.aggregates.push(unsafe { core::mem::transmute(desc) });
        
        return AggregateID {
            id: id,
            _mark: PhantomData,
        };
    }

    /// get the aggreagate descriptor
    pub fn get_aggregate<'ctx>(&'ctx self, id: AggregateID<'ctx>) -> &AggregateDesc {
        self.aggregates.get(id.id).expect("invalid aggregate id")
    }

    /// define an interface type
    pub fn declare_interface<'ctx>(&'ctx mut self, desc: InterfaceDesc<'ctx>) -> InterfaceID {
        // check if interface already declared
        for (i, iface) in self.interfaces.iter().enumerate(){
            if iface.hash == desc.hash{
                return InterfaceID{
                    id: i,
                    _mark: PhantomData
                }
            }
        }

        // get the id
        let id = self.interfaces.len();
        // push the interface
        self.interfaces.push(unsafe { core::mem::transmute(desc) });
        // return wrapped id
        return InterfaceID {
            id: id,
            _mark: PhantomData,
        };
    }

    /// get the interface descriptor
    pub fn get_interface<'ctx>(&'ctx self, id: InterfaceID<'ctx>) -> &InterfaceDesc {
        self.interfaces.get(id.id).expect("invalid interface id")
    }

    /// declare a function
    pub fn declare_function<'ctx, S: Into<String>>(
        &'ctx mut self,
        name: Option<S>,
        params: &[Type<'ctx>],
        return_ty: Type<'ctx>,
        is_async: bool,
        is_generator: Option<GeneratorDesc<'ctx>>,
        linkage: Option<Linkage>,
    ) -> FunctionID {
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

    /// define a function
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

    /// get the function if defined
    pub fn get_function<'ctx>(&'ctx self, id: FunctionID<'ctx>) -> Option<&'ctx Function> {
        self.functions.get(id.id).expect("invalid function id").function.as_ref()
    }

    /// get the function if defined
    pub fn get_function_by_name<'ctx>(&'ctx self, name: &str) -> Option<&'ctx Function>{
        for f in &self.functions{
            if let Some(n) = &f.name{
                if name == n{
                    return f.function.as_ref()
                }
            }
        }

        return None
    }

    pub fn get_function_type<'ctx>(&'ctx self, id: FunctionID<'ctx>) -> &'ctx FunctionType {
        &self.functions.get(id.id).expect("invalid function id").ty
    }

    pub fn create_function<'ctx>(&'ctx self, params: &[Type<'ctx>], return_ty: Type<'ctx>, is_async: bool, is_generator: Option<GeneratorDesc<'ctx>>) -> Function{
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

    pub fn compile<B: Backend>(&self, mut backend: B) -> Result<B::Output, String>{
        backend.compile(self)
    }
}