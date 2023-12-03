use core::marker::PhantomData;
use core::sync::atomic::{AtomicUsize, Ordering};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Size {
    Fixed(usize),
    PointerSize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AggregateID<'ctx> {
    pub(super) id: usize,
    pub(super) _mark: PhantomData<&'ctx ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InterfaceID<'ctx> {
    pub(super) id: usize,
    pub(super) _mark: PhantomData<&'ctx ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumID<'ctx> {
    pub(super) id: usize,
    pub(super) _mark: PhantomData<&'ctx ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockID(usize);

impl BlockID {
    pub(crate) fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(1);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueID(usize);

impl ValueID {
    pub(crate) fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(1);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackSlotID(pub(crate) usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionID<'ctx> {
    pub(crate) id: usize,
    pub(super) _mark: PhantomData<&'ctx ()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(u64);
