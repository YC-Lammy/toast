
pub mod types;

mod blocks;
mod ops;

use core::marker::PhantomData;
use core::sync::atomic::{AtomicU64, Ordering};

use types::{
    Type,
    FloatType,
    IntegerType,
};

use blocks::{
    Params,
    ValueList
};

use ops::{
    OP,
    Branch
};

static GLOBAL_ID: AtomicU64 = AtomicU64::new(0);

pub(crate) fn gen_id() -> u64{
    return GLOBAL_ID.fetch_add(1, Ordering::SeqCst)
}



struct StackSlotRecord{
    id: u64,
    size: usize,
}

struct BlockRecord{
    id: u64,
}

pub struct FunctionBuilder<P:Params, R:Type>{
    stack_slots: Vec<StackSlotRecord>,
    blocks: Vec<u64>,
    _d: PhantomData<(P, R)>
}

impl<P:Params, R:Type> FunctionBuilder<P, R>{
    pub const fn new() -> Self{
        Self {
            stack_slots: Vec::new(),
            blocks: Vec::new(),
            _d: PhantomData
        }
    }

    pub fn create_block<'builder, B:Params>(&'builder mut self) -> Block<'builder, B, P, R>{
        let id = gen_id();
        self.blocks.push(id);

        return Block::from_builder(self, id);
    }

    pub fn params<'builder>(&'builder self) -> &P::Values<'builder>{
        todo!()
    }

    pub fn validate(&self){

    }
}

pub struct Block<'builder, P:Params, FP:Params, FR:Type>{
    id: u64,
    builder: &'builder FunctionBuilder<FP, FR>,
    params: Vec<u64>,
    ops: Vec<OP>,
    _d: PhantomData<P>
}

impl<'builder, P:Params, FP:Params, FR:Type> Block<'builder, P, FP, FR>{
    fn from_builder(builder: &'builder FunctionBuilder<FP, FR>, id: u64) -> Self{
        let mut params = Vec::new();

        for _ in 0..P::len(){
            params.push(gen_id());
        }

        Self { 
            id, 
            builder,
            params: params,
            ops: Vec::new(),
            _d: PhantomData 
        }
    }

    pub fn block_params<'block>(&'block self) -> &P::Values<'block>{
        unsafe{core::mem::transmute(self.params.as_ptr())}
    }

    pub fn function_params(&self) -> &FP::Values<'builder>{
        self.builder.params()
    }

    pub fn stack_store<'block, T:Type>(&'block mut self, slot: StackSlot<'builder, T>, value: Value<'block, T>){
        todo!()
    }

    pub fn stack_load<'block, T:Type>(&'block mut self, slot: StackSlot<'builder, T>) -> Value<'block, T>{
        todo!()
    }

    pub fn load<'block, T:Type>(&'block mut self, ptr: Value<'block, *mut T>) -> Value<'block, T>{
        todo!()
    }

    pub fn store<'block, T:Type>(&'block mut self, ptr: Value<'block, *mut T>, value: Value<'block, T>){

    }

    pub fn add<'block, T:IntegerType>(&'block mut self, a: Value<'block, T>, b: Value<'block, T>) -> Value<'block, T>{
        let id = gen_id();

        self.ops.push(
            OP::Add { 
                a: a.id, 
                b: b.id, 
                result: id
            }
        );

        return Value { id: id, _d: PhantomData }
    }

    pub fn sub<'block, T:IntegerType>(&'block mut self, a: Value<'block, T>, b: Value<'block, T>) -> Value<'block, T>{
        let id = gen_id();

        self.ops.push(
            OP::Sub { 
                a: a.id, 
                b: b.id, 
                result: id
            }
        );

        return Value { id: id, _d: PhantomData }
    }

    pub fn return_<'block>(mut self, v:Value<'block, FR>){
        
    }

    pub fn brif<'block, T:Type, TP:Params, EP:Params>(
        &'block mut self, 
        test: Value<T>,
        then_block: &Block<'builder, TP, FP, FR>,
        then_params: TP::Values<'block>,
        else_block: &Block<'builder, EP, FP, FR>,
        else_params: EP::Values<'block>
    ){
        
    }

    pub fn switch<'block, T:IntegerType>(&'block mut self, value: Value<'block, T>) -> SwitchBuilder<'block, 'builder, T, P, FP, FR>{
        return SwitchBuilder { 
            block: self,
            value: value,
            branches: Vec::new(),
            _d: PhantomData
        }
    }


}

pub struct StackSlot<'builder, T:Type>{
    id: u64,
    _d: PhantomData<&'builder T>
}

#[repr(transparent)]
pub struct Value<'block, T:Type>{
    id: u64,
    _d: PhantomData<&'block T>
}

pub struct SwitchBuilder<'block, 'builder, T:IntegerType, P:Params, FP:Params, FR:Type>{
    block: &'block mut Block<'builder, P, FP, FR>,
    value: Value<'block, T>,
    branches: Vec<(u64, u64, Vec<u64>)>,
    _d: PhantomData<T>,
}

impl<'block, 'builder, T:IntegerType, P:Params, FP:Params, FR:Type> SwitchBuilder<'block, 'builder, T, P, FP, FR>{
    pub fn add_branch<BP:Params>(&mut self, value: Value<'block, T>, dest: &Block<'builder, BP, FP, FR>, args: BP::Values<'block>){
        let args = args.to_ids();
        self.branches.push((value.id, dest.id, args));
    }

    pub fn build<BP:Params>(self, default_dest: &Block<'builder, BP, FP, FR>, args: BP::Values<'block>){
        let args = args.to_ids();

        let mut branches = Vec::with_capacity(self.branches.len());

        for branch in self.branches{
            branches.push(
                Branch{
                    value_id: branch.0,
                    blcok_id: branch.1,
                    args: branch.2.into()
                }
                
            );
        }

        self.block.ops.push(
            OP::Switch { 
                value: self.value.id, 
                branches: branches.into(), 
                default_block_id: default_dest.id, 
                default_block_args: args.into()
            }
        )

    }
}