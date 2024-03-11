

#[global_allocator]
static ALLOCATOR: LibcAllocator = LibcAllocator;

pub struct LibcAllocator;

unsafe impl alloc::alloc::GlobalAlloc for LibcAllocator {
    unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
        let size = layout.size();
        libc::malloc(size) as *mut u8
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _layout: core::alloc::Layout) {
        libc::free(ptr as _);
    }

    unsafe fn realloc(
        &self,
        ptr: *mut u8,
        _layout: core::alloc::Layout,
        new_size: usize,
    ) -> *mut u8 {
        libc::realloc(ptr as _, new_size) as _
    }
}