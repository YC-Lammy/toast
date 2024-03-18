use std::marker::PhantomData;

use crate::context::GeneratorDesc;
use crate::mir::MIR;
use crate::types::*;
use crate::util::*;

pub(crate) struct SSA<'ctx> {
    pub id: ValueID,
    pub ty: Type<'ctx>,
}

pub(crate) struct LandingPadDesc<'ctx>{
    pub(crate) personality: FunctionID<'ctx>,
    pub(crate) clauses: Vec<ValueID>,
}

pub(crate) struct BlockDesc<'ctx> {
    pub(crate) id: BlockID,
    pub(crate) landing_pad: Option<LandingPadDesc<'ctx>>,
    pub(crate) inst: Vec<MIR<'ctx>>,
}

impl<'ctx> BlockDesc<'ctx> {
    pub fn new(id: BlockID, params: &[Type<'ctx>]) -> Self {
        let mut param_values = Vec::new();
        let mut values = Vec::new();

        for i in 0..params.len() {
            let id = ValueID::new();

            param_values.push(id);
            values.push((id, params[i].clone()));
        }

        Self {
            id,
            landing_pad: None,
            inst: Vec::new(),
        }
    }
}

pub struct Function<'ctx> {
    pub(crate) params: Vec<Type<'ctx>>,
    pub(crate) return_: Type<'ctx>,
    pub(crate) is_async: bool,
    pub(crate) is_generator: Option<GeneratorDesc<'ctx>>,
    pub(crate) map_ssa_func: Vec<(FunctionID<'ctx>, ValueID)>,
    pub(crate) blocks: Vec<BlockDesc<'ctx>>,
    pub(crate) stackslots: Vec<Type<'ctx>>,
    pub(crate) _mark: PhantomData<&'ctx ()>,
}

impl<'ctx> Function<'ctx> {
    pub fn new(
        params: &[Type<'ctx>],
        return_ty: Type<'ctx>,
        is_async: bool,
        is_generator: Option<GeneratorDesc<'ctx>>,
    ) -> Self {
        Self {
            params: params.to_vec(),
            return_: return_ty,
            is_async: is_async,
            is_generator: is_generator,
            map_ssa_func: Vec::new(),
            blocks: Vec::new(),
            stackslots: Vec::new(),
            _mark: PhantomData,
        }
    }

    /// return true if function is async
    pub fn is_async(&self) -> bool {
        self.is_async
    }

    /// return true if fucntion is generator
    pub fn is_generator(&self) -> bool {
        self.is_generator.is_some()
    }

    /// return the resume type if function is generator
    pub fn generator_resume_type(&self) -> Option<Type<'ctx>> {
        self.is_generator.as_ref().map(|w| w.resume_type.clone())
    }

    /// return the yield type if function is generator
    pub fn generator_yield_type(&self) -> Option<Type<'ctx>> {
        self.is_generator.as_ref().map(|w| w.yield_type.clone())
    }
}
