
use iron_gc::{GcPtr, GcCell};

use super::{JSValue, Any, JSString};

pub struct Bigint(GcPtr<i128>);



impl JSValue for Bigint{
    fn data_bits(&self) -> u64 {
        self.0.as_ptr() as u64
    }

    fn type_tag(&self) -> u64 {
        Any::BIGINT_TAG
    }

    fn from_any(any:Any) -> Self {
        Bigint(unsafe{core::mem::transmute(any.data() as usize)})
    }
}

impl Bigint {
    pub fn as_gc_ptr(&self) -> GcPtr<i128>{
        self.0
    }
    
    pub fn to_string(&self) -> JSString{
        todo!()
    }
}