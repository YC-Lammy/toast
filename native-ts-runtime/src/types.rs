use core::ptr::NonNull;

use libobject::TypeInfo;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Any {
    /// the type id of the type
    pub type_info: TypeInfo<'static>,
    /// pointer to object
    pub obj: *const u8,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Function {
    /// pointer to the function
    pub function: *const u8,
    /// context, only for clousures
    pub context: Option<NonNull<u8>>,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct Iterator {
    /// the type id of the type
    pub type_info: TypeInfo<'static>,
    /// pointer to object
    pub obj: *const u8,
    /// the next function
    pub next: Function,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Promise{
    handle: crate::asynchronous::executor::TaskHandle<'static>
}