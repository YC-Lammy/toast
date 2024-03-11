use core::ptr::NonNull;

pub fn allocate<T>(value: T) -> NonNull<T> {
    todo!()
}

pub unsafe fn allocate_raw(size: usize) -> NonNull<u8> {
    todo!()
}
