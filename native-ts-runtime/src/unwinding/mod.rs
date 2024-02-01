

// References:
// https://github.com/rust-lang/rust/blob/c4be230b4a30eb74e3a3908455731ebc2f731d3d/library/panic_unwind/src/gcc.rs
// https://github.com/rust-lang/rust/blob/c4be230b4a30eb74e3a3908455731ebc2f731d3d/library/panic_unwind/src/dwarf/eh.rs
// https://docs.rs/gimli/0.25.0/src/gimli/read/cfi.rs.html
mod arch;
mod personality;
mod panic;
mod panicking;

use core::mem::ManuallyDrop;

pub use personality::rust_eh_personality;
pub use panic::begin_panic;

use crate::types::{Any, JSFunc, HeapVariable};

static mut ERROR: Any = Any::UNDEFINED;

pub fn throw(exception: Any) -> !{
    unsafe{
        ERROR = exception;
    }
    panic!("uncaught error: {}", exception.to_string().as_str());
}

pub fn try_call(func: JSFunc, this: Any, captures: *const HeapVariable, argc: u32, args: *const Any) -> Result<Any, Any>{
    struct CallData{
        func: JSFunc,
        this: Any,
        captures: *const HeapVariable,
        argc: u32,
        args: *const Any
    }

    fn do_call(data: *mut u8){
        unsafe{
            let data = (data as *mut CallData).read();
            ERROR = data.func.dynamic_call(data.this, data.captures, data.argc, data.args);
        }
    }

    fn do_catch(data: *mut u8, exception: *mut u8){

    }

    let data = ManuallyDrop::new(CallData{
        func,
        this,
        captures,
        argc,
        args
    });

    unsafe{
        if core::intrinsics::r#try(do_call, &data as *const _ as *mut u8, do_catch) == 0{
            drop(ManuallyDrop::into_inner(data));
            return Ok(ERROR)

        } else{

            drop(ManuallyDrop::into_inner(data));
            return Err(ERROR)
        }
    };
}

pub unsafe fn read_exception(exception: *mut u8) -> Any{
    return ERROR
}