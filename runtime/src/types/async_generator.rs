use alloc::vec::Vec;

use iron_gc::GcPtr;
use iron_gc::Array;

use super::Any;



pub static mut ASYNC_GENERATORS: Vec<AsyncGenerator> = Vec::new();

pub struct AsyncGenerator{

}

impl AsyncGenerator{
    pub fn register(func:*const (), captures:Option<GcPtr<Array<Any>>>, this:Any, args:&[Any]){

    }

    pub fn resume(&mut self) -> bool{
        false
    }
}