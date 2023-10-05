use core::hash::Hash;
use std::hash::Hasher;


pub use ahash;

/// a zero allocation function for hashing a usize as string
#[inline]
pub fn hash_integer(value: usize) -> u64{
    let mut buf = itoa::Buffer::new();
    let s = buf.format(value);

    let mut hasher = ahash::AHasher::default();
    s.hash(&mut hasher);

    return hasher.finish();
}

/// a fast hasing method for hashing string
#[inline]
pub fn hash_string(s: &str) -> u64{
    let mut hasher = ahash::AHasher::default();
    s.hash(&mut hasher);

    return hasher.finish();
}