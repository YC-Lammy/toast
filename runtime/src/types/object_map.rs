

use alloc::vec::Vec;
use iron_gc::GcPtr;

use super::{JSString, Any};

lazy_static::lazy_static!{
    static ref ROOT: GcPtr<TypeMap> = {
        let ptr = GcPtr::new(
            TypeMap { 
                hash: 0,
                index: 0,
                key: JSString::new("constructor"),
                parent: None,
                childrens: Vec::new()
            }
        );
        // root Type Map
        ptr.set_root();

        ptr
    };
}


#[repr(C)]
pub struct TypeMap{
    hash: u64,
    index: usize,
    key: JSString,
    parent: Option<GcPtr<TypeMap>>,
    childrens: Vec<(u64, GcPtr<TypeMap>)>,
}

impl iron_gc::Trace for TypeMap{
    fn trace(&mut self, visitor: &mut iron_gc::Visitor) {
        visitor.visit(self.key.0);
        
        if let Some(p) = self.parent{
            visitor.visit(p);
        }

        for (_, child) in &self.childrens{
            visitor.visit(*child);
        }
    }
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
        } else{
            let hash = native_js_common::hash_integer(index);
            return self.get_by_hash(hash)
        }
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

    /// extending an object is costly
    pub fn set(&mut self, key: &JSString, value: Any){
        if let Some(index) = self.type_map.get_index_by_hash(key.hash()){
            let index = index + self.array_length;
            self.values[index] = value;

        } else{
            if key.as_str().is_ascii(){
                if let Some(idx) = atoi::atoi::<usize>(key.as_slice()){

                    if idx == self.array_length{
                        self.values.insert(idx, value);
                        self.array_length += 1;
                        return;
                    }

                    if idx < self.array_length{
                        self.values[idx] = value;
                        return;
                    }

                    if idx < self.array_length + 64{
                        let extend_length = idx.abs_diff(self.array_length) + 1;
                        self.values.resize(self.values.len() + extend_length, Any::UNDEFINED);

                        self.values.copy_within(self.array_length.., self.array_length + extend_length);
                        
                        self.array_length += extend_length;
                        self.values[idx] = value;

                        return;
                    }
                }
            };

            let child = self.type_map.make_child(*key);

            let index = child.index + self.array_length;

            self.type_map = child;

            self.values.push(value);
        }
    }

    pub fn push(&mut self, value: Any){
        let idx = self.array_length;
        self.array_length += 1;
        
        if self.values.len() == idx{
            self.values.push(value);
        } else{
            self.values.insert(idx, value);
        }
    }
}

impl iron_gc::Trace for ObjectMap{
    fn trace(&mut self, visitor: &mut iron_gc::Visitor) {
        visitor.visit(self.type_map);

        for v in &self.values{
            v.trace(visitor);
        }
    }
}