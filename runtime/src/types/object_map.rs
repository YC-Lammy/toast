

use alloc::vec::Vec;
use iron_gc::GcPtr;

use super::{JSString, Any};

lazy_static::lazy_static!{
    static ref ROOT: GcPtr<TypeMap> = GcPtr::new(
        TypeMap { 
            hash: 0,
            index: 0,
            key: JSString::new("constructor"),
            parent: None,
            childrens: Vec::new()
        }
    );
}

#[repr(C)]
pub struct TypeMap{
    hash: u64,
    index: usize,
    key: JSString,
    parent: Option<GcPtr<TypeMap>>,
    childrens: Vec<(u64, GcPtr<TypeMap>)>,
}

impl TypeMap{
    fn append_key(&self, v: &mut Vec<JSString>){
        if let Some(p) = &self.parent{
            p.append_key(v);
        }
        v.push(self.key);
    }

    pub fn keys(&self) -> Vec<JSString>{
        let mut keys = Vec::new();
        self.append_key(&mut keys);

        return keys
    }

    pub fn get_index_by_hash(&self, hash: u64) -> Option<usize>{
        if self.hash == hash{
            return Some(self.index)
        }
        if let Some(parent) = &self.parent{
            return parent.get_index_by_hash(hash)
        }
        return None
    }

    pub fn make_child(&mut self, key: JSString) -> GcPtr<TypeMap>{
        
        let hash = key.hash();

        for child in &self.childrens{
            if child.0 == hash{
                return child.1;
            }
        }
        let index = self.index + 1;

        let self_ptr = unsafe{GcPtr::from_ptr(self)};

        let c = GcPtr::new(
            TypeMap{
                hash,
                index,
                key,
                parent: self_ptr,
                childrens: Vec::new()
            }
        );

        self.childrens.push((hash, c));

        return c
    }
}

pub struct ObjectMap{
    type_map: GcPtr<TypeMap>,
    array_length: usize,
    values: Vec<Any>
}

impl ObjectMap{
    pub fn new() -> ObjectMap{
        ObjectMap { 
            type_map: *ROOT, 
            array_length: 0, 
            values: Vec::new()
        }
    }

    pub fn get_index(&self, index: usize) -> Option<Any>{
        if index < self.array_length{
            return Some(self.values[index])
        }

        index.as_s
    }

    pub fn get(&mut self, key: &JSString) -> Option<Any>{
        return self.get_by_hash(key.hash())
    }

    pub fn get_by_hash(&self, hash: u64) -> Option<Any>{
        if let Some(index) = self.type_map.get_index_by_hash(hash){
            let index = index + self.array_length;
            return Some(self.values[index])
        }

        return None;
    }

    pub fn set(&mut self, key: &JSString, value: Any){
        if let Some(index) = self.type_map.get_index_by_hash(key.hash()){
            let index = index + self.array_length;
            self.values[index] = value;
        } else{
            let child = self.type_map.make_child(*key);

            let index = child.index + self.array_length;

            self.type_map = child;

            self.values.push(value);
        }
    }
}