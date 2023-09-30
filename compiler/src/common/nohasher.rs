use std::collections::hash_map::RandomState;
use std::collections::HashSet;
use std::hash::{BuildHasher, BuildHasherDefault, Hasher};

#[derive(Default)]
pub struct NoHasher {
    value: u64,
}

impl Hasher for NoHasher {
    fn write(&mut self, bytes: &[u8]) {
        if self.value != 0 {
            let mut h = std::collections::hash_map::DefaultHasher::default();
            h.write(bytes);
            self.value = h.finish();
        } else {
            let mut h = std::collections::hash_map::DefaultHasher::default();
            h.write_u64(self.value);
            h.write(bytes);
            self.value = h.finish();
        }
    }

    fn write_usize(&mut self, i: usize) {
        self.value = i as u64
    }

    fn write_u128(&mut self, i: u128) {
        self.value = i as u64
    }

    fn write_u64(&mut self, i: u64) {
        self.value = i as u64
    }

    fn write_u32(&mut self, i: u32) {
        self.value = i as u64
    }

    fn write_u16(&mut self, i: u16) {
        self.value = i as u64
    }

    fn write_u8(&mut self, i: u8) {
        self.value = i as u64
    }

    fn write_isize(&mut self, i: isize) {
        self.value = i as u64
    }

    fn write_i128(&mut self, i: i128) {
        self.value = i as u64
    }

    fn write_i64(&mut self, i: i64) {
        self.value = i as u64
    }

    fn write_i32(&mut self, i: i32) {
        self.value = i as u64
    }

    fn write_i16(&mut self, i: i16) {
        self.value = i as u64
    }

    fn write_i8(&mut self, i: i8) {
        self.value = i as u64
    }

    fn finish(&self) -> u64 {
        return self.value;
    }
}

impl BuildHasher for NoHasher {
    type Hasher = NoHasher;
    fn build_hasher(&self) -> Self::Hasher {
        return NoHasher { value: 0 };
    }
}

#[derive(Debug, Clone)]
pub struct HHashSet<T, S = RandomState> {
    pub build_hasher: S,
    pub hashset: HashSet<T, BuildHasherDefault<NoHasher>>,
}

impl<T, S> AsRef<HashSet<T, BuildHasherDefault<NoHasher>>> for HHashSet<T, S> {
    fn as_ref(&self) -> &HashSet<T, BuildHasherDefault<NoHasher>> {
        &self.hashset
    }
}

impl<T, S> AsMut<HashSet<T, BuildHasherDefault<NoHasher>>> for HHashSet<T, S> {
    fn as_mut(&mut self) -> &mut HashSet<T, BuildHasherDefault<NoHasher>> {
        &mut self.hashset
    }
}

impl<T, S> HHashSet<T, S> where S: BuildHasher {}
