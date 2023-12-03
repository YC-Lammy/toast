use crate::mir::MIR;
use crate::types::*;
use crate::util::*;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Linkage {
    Private,
    Internal,
}

pub(crate) struct BlockDesc<'ctx> {
    pub(crate) id: BlockID,
    pub(crate) values: Vec<(ValueID, Type<'ctx>)>,
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
            values: values,
            inst: Vec::new(),
        }
    }

    pub fn new_id(&mut self, ty: Type<'ctx>) -> ValueID {
        let id = ValueID::new();
        self.values.push((id, ty));
        return id;
    }
}

pub struct Function<'ctx> {
    pub(crate) blocks: Vec<BlockDesc<'ctx>>,
    pub(crate) stackslots: Vec<Type<'ctx>>,
    _mark: &'ctx (),
}

impl<'ctx> Function<'ctx> {}
