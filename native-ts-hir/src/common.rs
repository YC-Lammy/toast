use core::sync::atomic::{AtomicUsize, Ordering};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(pub(super) usize);

impl FunctionId {
    pub fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ClassId(pub(super) usize);

impl ClassId {
    pub fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InterfaceId(pub(super) usize);

impl InterfaceId {
    pub fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableId(pub(super) usize);

impl VariableId {
    pub fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericId(pub(super) usize);

impl GenericId {
    pub fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AliasId(pub(super) usize);

impl AliasId {
    pub fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumId(pub(super) usize);

impl EnumId {
    pub fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub(super) usize);

impl ModuleId {
    pub fn new() -> Self {
        static COUNT: AtomicUsize = AtomicUsize::new(0);
        Self(COUNT.fetch_add(1, Ordering::SeqCst))
    }
}
