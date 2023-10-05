
use iron_gc::{
    GcPtr,
    Array
};

use crate::types::Any;

type AsyncFunction = extern "C" fn(*const (), usize) -> Any;

#[repr(C)]
pub struct AsyncRoutine{
    pub(crate) count: usize,
    pub(crate) data: GcPtr<Array<Any>>,
    pub(crate) func: *const ()
}

impl AsyncRoutine{
    pub fn new(func: *const (), data: GcPtr<Array<Any>>) -> Self{
        return Self { 
            count: 0, 
            data: data, 
            func: func
        }
    }
}

#[repr(C)]
pub struct Promise{

}


pub fn spawn(routine: AsyncRoutine) -> Promise{
    // make sure data is root
    routine.data.set_root();


    return Promise {}
}