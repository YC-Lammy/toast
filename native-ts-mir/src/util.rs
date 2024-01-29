use core::marker::PhantomData;
use core::sync::atomic::{AtomicUsize, Ordering};
use std::hash::Hash;
use std::hash::Hasher;

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
pub struct BlockID(usize);

impl BlockID {
    pub(crate) fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(1);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueID(pub(crate) usize);

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

impl Ident {
    /// from string
    pub fn from_str(s: &str) -> Self {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();

        hasher.write_u8(1);
        s.hash(&mut hasher);

        return Self(hasher.finish());
    }

    /// from index
    pub fn from_index(i: usize) -> Self {
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        hasher.write_u8(0);
        hasher.write_usize(i);

        Self(hasher.finish())
    }
}
