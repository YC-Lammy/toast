#![no_std]

pub mod fxhash;

extern crate alloc;

pub use dtoa as ftoa;
pub use itoa;

/// a zero allocation function for hashing a usize as string
#[inline]
pub fn hash_integer(value: usize) -> u64{
    let mut buf = itoa::Buffer::new();
    let s = buf.format(value);

    return hash_string(s);
}

#[inline]
pub fn hash_float(value: f64) -> u64{
    let mut buf = dtoa::Buffer::new();

    let s = buf.format(value);

    return hash_string(s)
}

/// a fast hasing method for hashing string
#[inline]
pub fn hash_string(s: &str) -> u64{
    return fxhash::hash64(&s);
}