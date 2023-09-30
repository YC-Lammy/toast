use std::{
    collections::HashMap,
    hash::{Hash, Hasher},
};

use parking_lot::RwLock;

use super::NoHasher;

lazy_static::lazy_static! {
    static ref ID_REF: RwLock<HashMap<u64, String, NoHasher>> = RwLock::new(HashMap::default());
}

#[derive(Hash, PartialEq, Eq)]
pub struct CachedString(u64);

impl CachedString {
    pub fn new<T>(value: T) -> Self
    where
        T: Hash + Into<String>,
    {
        let mut hasher = std::collections::hash_map::DefaultHasher::default();
        value.hash(&mut hasher);
        let id = hasher.finish();
        let map = ID_REF.read();
        if !map.contains_key(&id) {
            drop(map);

            let mut map = ID_REF.write();
            map.insert(id, value.into());
        }

        return CachedString(id);
    }

    pub fn as_str<'a>(&'a self) -> &'a str {
        let map = ID_REF.read();
        let s = map.get(&self.0).unwrap();
        let s = s.as_str();
        return unsafe { std::mem::transmute_copy(&s) };
    }
}

impl Into<String> for CachedString {
    fn into(self) -> String {
        self.as_str().to_string()
    }
}

impl std::fmt::Display for CachedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl AsRef<str> for CachedString {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl From<String> for CachedString {
    fn from(value: String) -> Self {
        Self::new(value)
    }
}
