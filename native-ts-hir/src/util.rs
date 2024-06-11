pub enum OR<A, B> {
    A(A),
    B(B),
}

pub enum MaybeTranslated<T> {
    NotTranslated,
    Translated(T),
}

impl<T> MaybeTranslated<T> {
    pub fn expect(&self, msg: &str) -> &T {
        match self {
            Self::NotTranslated => panic!("not yet translated: {}", msg),
            Self::Translated(t) => t,
        }
    }
    pub fn expect_mut(&mut self, msg: &str) -> &mut T {
        match self {
            Self::NotTranslated => panic!("not yet translated: {}", msg),
            Self::Translated(t) => t,
        }
    }
}

impl<T> Clone for MaybeTranslated<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Self::NotTranslated => Self::NotTranslated,
            Self::Translated(t) => Self::Translated(t.clone()),
        }
    }
}

impl<T> Copy for MaybeTranslated<T> where T: Copy {}

impl<T> core::fmt::Debug for MaybeTranslated<T>
where
    T: core::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NotTranslated => f.write_str("MaybeTranslated::NotTranslated"),
            Self::Translated(t) => t.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TotalF64(pub f64);

impl From<f64> for TotalF64 {
    fn from(value: f64) -> Self {
        Self(value)
    }
}

impl PartialEq for TotalF64 {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}

impl Eq for TotalF64 {}

impl PartialOrd for TotalF64 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.total_cmp(&other.0))
    }
}
impl Ord for TotalF64 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.total_cmp(&other.0)
    }
}

impl std::hash::Hash for TotalF64 {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u8(0x47);
        state.write_u64(self.0.to_bits());
    }
}
