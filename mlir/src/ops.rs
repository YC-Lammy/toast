
pub struct Branch{
    pub value_id: u64,
    pub blcok_id: u64,
    pub args: Box<[u64]>,
}

pub enum OP{
    StackStore{
        stack_slot_id: u64,
        value_id: u64,
    },
    StackLoad{
        stack_slot_id: u64,
        result_id: u64,
    },
    Store{
        ptr_id: u64,
        value_id: u64,
    },
    Load{
        ptr_id: u64,
        result_id: u64,
    },
    Add{
        a: u64,
        b: u64,
        result: u64,
    },
    Sub{
        a: u64,
        b: u64,
        result: u64,
    },
    Switch{
        value: u64,
        branches: Box<[Branch]>,
        default_block_id: u64,
        default_block_args: Box<[u64]>
    }
}