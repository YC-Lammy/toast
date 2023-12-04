extern crate alloc;

mod heap;
mod cell;

use cell::Flags;

pub struct Allocator{
    
}

impl Allocator{
    pub fn is_gc_marking_phase(&self) -> bool{
        false
    }
    pub fn is_grey_scanning_phase(&self) -> bool{
        false
    }
}

static ALLOCATOR: Allocator = Allocator{

};

#[no_mangle]
#[export_name = "runtime.gc_safe_point"]
pub extern "C" fn safe_point(){
    while ALLOCATOR.is_grey_scanning_phase() {
        
    }
}

#[no_mangle]
#[export_name = "runtime.gc_write_barrier"]
pub unsafe extern "C" fn write_barrier(ptr: &'static mut cell::Cell, slot: &'static mut *mut cell::Cell, value: &'static mut cell::Cell){
    // a possible safe point
    safe_point();
    if ALLOCATOR.is_gc_marking_phase(){
        ptr.header.flags = ptr.header.flags & Flags::GREY;
        value.header.flags = value.header.flags & Flags::GREY;
    }
    *slot = value;
}

#[no_mangle]
#[export_name = "runtime.gc_allocate"]
pub unsafe fn allocate(size: usize) -> &'static mut cell::Cell{
    // a possible safe point
    safe_point();
    todo!();
}