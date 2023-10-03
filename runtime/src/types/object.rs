
use alloc::boxed::Box;
use alloc::string::String;
use hashbrown::HashMap;
//use crate::bdwgc;

use iron_gc::GcPtr;

use super::{promise::Promise, *};

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct Object{
    pub(crate) inner: GcPtr<ObjectInner>
}

#[repr(C)]
pub struct ObjectInner{
    /// reference count
    rc: u32,

    /// internal value
    value: ObjectValue,

    __proto__: Option<Object>,

    structure: *mut ObjectStructure,

    /// capacity of array
    cap: usize,
    /// number of indexed properties
    len: usize,
    /// number of named properties
    named_properties: usize,
    values: GcPtr<Any>,
}

unsafe impl Sync for ObjectInner{}



pub enum ObjectValue {
    Empty,
    Function(TSFuncPtr),
    FunctionBind(FunctionBind),
    Closure(Closure),
    AsyncFunction(Closure),
    GeneratorFunction(Closure),
    AsyncGeneratorFunction(Closure),
    Generator(),
    Boolean(bool),
    Symbol(JSSymbol),
    Number(f64),
    Promise(Promise),
}

impl JSValue for Object{
    fn data_bits(&self) -> u64 {
        self.inner.to_raw_ptr() as u64
    }
    fn type_tag(&self) -> u64 {
        Any::OBJECT_TAG
    }
    fn from_any(any:Any) -> Self {
        Object { inner: unsafe{GcPtr::from_raw_ptr(any.data() as usize as _)} }
    }
}

pub struct ObjectStructure{
    /// key, index
    keys: HashMap<u64, usize>,
    added_key: String,
    branches: HashMap<u64, *mut ObjectStructure>
}

unsafe impl Sync for ObjectStructure{}

lazy_static::lazy_static!{
    static ref NULL_OBJECT_STRUCTURE:&'static mut ObjectStructure = {
        let mut o = Box::leak(Box::new(ObjectStructure{
            keys: HashMap::default(),
            added_key: String::from("constructor"),
            branches: HashMap::default()
        }));

        o = o.add_property("".into());

        o
    };
}

impl ObjectStructure{
    pub fn from_ptr(ptr:*const Self) -> &'static mut Self{
        unsafe{(ptr as *mut Self).as_mut().unwrap_unchecked()}
    }

    pub fn as_mut(&self) -> &'static mut Self{
        unsafe{(self as *const Self as *mut Self).as_mut().unwrap_unchecked()}
    }

    pub fn get_property(&self, property:u64) -> Option<usize>{
        if let Some(k) = self.keys.get(&property){
            return Some(*k)
        } else{
            return None
        }
    }

    pub fn add_property(&mut self, property:&str) -> &'static mut ObjectStructure{
        let key = JSString::hash_key_from_utf8(&property);

        if let Some(b) = self.branches.get(&key){
            return Self::from_ptr(*b)
        } else{
            let mut k = self.keys.clone();
            k.insert(key, k.len());

            let s = Box::leak(Box::new(
                ObjectStructure{
                    keys: k,
                    added_key: String::from(property),
                    branches: HashMap::new()
                }
            ));

            self.branches.insert(key, s);
            return s
        }
    }
}

impl Object {
    #[inline]
    pub fn new() -> Self {
        let p = GcPtr::new(
            ObjectInner { 
                rc: 0,
                
                value: ObjectValue::Empty, 
                __proto__: None,
                structure: NULL_OBJECT_STRUCTURE.as_mut(), 
                cap: 16, 
                len: 0, 
                named_properties: 0, 
                values: GcPtr::new_array(&[Any::UNDEFINED;16])
            }
        );
        
        return Self{ inner: p }
    }

    #[inline]
    pub fn set_internal(&mut self, value: ObjectValue){
        self.inner.value = value;
    }

    #[inline]
    pub fn as_promise(&mut self) -> Option<&mut Promise>{
        match &mut self.inner.value{
            ObjectValue::Promise(p) => Some(p),
            _ => None
        }
    }

    #[inline]
    pub fn get_property(&self, key: JSString) -> Option<Any> {
        let hkey = key.to_hash_key();

        // fast path
        if let Some(offset) = ObjectStructure::from_ptr(self.inner.structure).get_property(hkey){
            let index = self.inner.len + offset;
            unsafe{
                let v = self.inner.values.as_ptr().add(index).read();
                return Some(v)
            }
        } else{
            // try to parse key into index
            let s = key.as_str();

            if s.is_ascii(){
                if let Some(idx) = atoi::atoi::<usize>(s.as_bytes()){
                    if idx < self.inner.len{
                        unsafe{
                            let v = self.inner.values.as_ptr().add(idx).read();
                            return Some(v)
                        }
                    }
                }
            }

            // try to read it from prototype
            if let Some(proto) = self.inner.__proto__{
                return proto.get_property(key)
            }
            return None
        }
    }

    #[inline]
    pub fn get_property_key(&self, key: u64) -> Option<Any> {
        if let Some(offset) = ObjectStructure::from_ptr(self.inner.structure).get_property(key){
            let index = self.inner.len + offset;
            unsafe{
                let v = self.inner.values.as_ptr().add(index).read();
                return Some(v)
            }
        }
        return None
    }

    #[inline]
    pub fn get_property_symbol(&self, key: JSSymbol) -> Option<Any> {
        return self.get_property_key(key.0);
    }

    fn grow(&mut self){
        let new_cap = self.inner.cap * 2;
        self.inner.cap = new_cap;
        unsafe{
            let new:GcPtr<Any> = GcPtr::malloc_array(new_cap);
            core::ptr::copy_nonoverlapping(self.inner.values.as_ptr(), new.as_mut(), self.inner.len + self.inner.named_properties);
            self.inner.values = new;
        }
    }

    #[inline]
    pub fn set_property(&mut self, key: JSString, value: Any) {
        
        // the write barriar for object
        if let Some(obj) = value.as_object(){
            self.inner.write_barriar(obj.inner);

        };

        // write barriar for string
        if let Some(s) = value.as_string(){
            match s{
                JSString::Alloc(a) => {
                    self.inner.write_barriar(a);
                }
                _ => {}
            }
        }

        // write barriar for bigint
        if let Some(b) = value.as_bigint(){
            self.inner.write_barriar(b.as_gc_ptr());
        }

        // calculate the hash key
        let hkey = key.to_hash_key();

        // the property is found
        if let Some(offset) = ObjectStructure::from_ptr(self.inner.structure).get_property(hkey){
            let index = self.inner.len + offset;
            unsafe{
                self.inner.values.as_mut().add(index).write(value);
            }

        } else{
            // try to parse key into index
            let s = key.as_str();

            // only ascii can be parsed to number
            if s.is_ascii(){
                // fast parsing
                if let Some(idx) = atoi::atoi::<usize>(s.as_bytes()){
                    if idx < self.inner.len{
                        unsafe{
                            self.inner.values.as_mut().add(idx).write(value);
                        }

                        return;

                    } else{
                        let d = self.inner.len - idx;

                        // the number of empty slots is less then 32
                        if d < 32{
                            
                            // push undefined
                            for _ in 0..d{
                                self.push(Any::UNDEFINED);
                            };

                            // push the actual value
                            self.push(value);

                            return;
                        }
                    }
                }
            };

            // fallback

            // extend current structure
            self.inner.structure = ObjectStructure::from_ptr(self.inner.structure).add_property(s);
            self.inner.named_properties += 1;

            // overflow resize required
            if self.inner.named_properties + self.inner.len > self.inner.cap{
                // resize array
                self.grow();
            }

            // calculate offset
            let idx = self.inner.len + self.inner.named_properties -1;

            unsafe{
                self.inner.values.as_mut().add(idx).write(value);
            }
        }
    }

    pub fn push(&mut self, value:Any){
        if self.inner.len + self.inner.named_properties >= self.inner.cap{
            self.grow();
        }

        let idx = self.inner.len;
        self.inner.len += 1;

        unsafe{
            if self.inner.named_properties != 0{
                core::ptr::copy(self.inner.values.as_ptr().add(idx), self.inner.values.as_mut().add(idx + 1), self.inner.named_properties);
            }

            self.inner.values.as_mut().add(idx).write(value);
        }; 
    }

    #[inline]
    pub fn call(&self, this:Any, args:&[Any]) -> Any{
        match &self.inner.value{
            ObjectValue::Function(f) => f.dynamic_call(this, 0 as _, args.len() as _, args.as_ptr()),
            ObjectValue::Closure(c) => c.call(this, args),
            ObjectValue::FunctionBind(f) => f.call(args),
            ObjectValue::AsyncFunction(c) => {
                todo!()
            }
            ObjectValue::GeneratorFunction(c) => {
                todo!()
            }
            ObjectValue::AsyncGeneratorFunction(g) => {
                todo!()
            }
            _ => unsafe{
                rt::RT_throw(Any::error("Call on non function object"));
                return Any::UNDEFINED
            }
        }
    }
}

#[test]
fn test_obj(){
    panic!("{}", core::mem::size_of::<ObjectInner>())
}