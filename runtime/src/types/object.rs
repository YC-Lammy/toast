
use alloc::boxed::Box;
use alloc::string::String;
use hashbrown::HashMap;
//use crate::bdwgc;

use iron_gc::GcPtr;

use crate::asynchronous::Promise;

use super::*;
use super::object_map::ObjectMap;
use super::regex::Regexp;

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct Object{
    pub(crate) inner: GcPtr<ObjectInner>
}

#[repr(C)]
pub struct ObjectInner{
    /// internal value
    value: ObjectValue,

    __proto__: Option<Object>,

    values: ObjectMap,
}

unsafe impl Sync for ObjectInner{}

impl iron_gc::Trace for ObjectInner{
    fn trace(&mut self, visitor: &mut iron_gc::Visitor) {
        self.values.trace(visitor);
        
        
    }
}

pub enum ObjectValue {
    Empty,
    Function(TSFuncPtr),
    FunctionBind(FunctionBind),
    Closure(Closure),
    AsyncFunction(Closure),
    GeneratorFunction(Closure),
    AsyncGeneratorFunction(Closure),
    Generator(),
    RegEx(Regexp),
    Promise(Promise),
    Boolean(bool),
    Symbol(JSSymbol),
    Number(f64),
}

impl JSValue for Object{
    fn data_bits(&self) -> u64 {
        self.inner.to_raw_ptr() as u64
    }
    fn type_tag(&self) -> u64 {
        Any::OBJECT_TAG
    }
    fn from_any(any:Any) -> Self {
        Object { inner: unsafe{GcPtr::<ObjectInner>::from_raw_ptr(any.data() as usize as _).expect("invalid pointer casted from Any.")} }
    }
}

impl Object {
    #[inline]
    pub fn new() -> Self {
        let p = GcPtr::new(
            ObjectInner { 
                value: ObjectValue::Empty, 
                __proto__: None,
                values: ObjectMap::new()
            }
        );
        
        return Self{ inner: p }
    }

    pub fn new_promise(promise: Promise) -> Self{
        let p = GcPtr::new(
            ObjectInner { 
                value: ObjectValue::Promise(promise), 
                __proto__: None,
                values: ObjectMap::new()
            }
        );
        
        return Self{ inner: p }
    }

    pub fn new_regex(reg: Regexp) -> Self{
        let p = GcPtr::new(
            ObjectInner { 
                value: ObjectValue::RegEx(reg), 
                __proto__: None,
                values: ObjectMap::new()
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
        let hkey = key.hash();

        // fast path
        if let Some(value) = self.inner.values.get(&key){
            return Some(value)

        } else{
            // try to parse key into index
            let s = key.as_str();

            if s.is_ascii(){
                if let Some(idx) = atoi::atoi::<usize>(s.as_bytes()){
                    if let Some(v) = self.inner.values.get_index(idx){
                        return Some(v)
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

    /// hash must not be generated from an integer string
    pub fn get_by_hash(&self, hash: u64) -> Option<Any>{
        if let Some(value) = self.inner.values.get_by_hash(hash){
            return Some(value)
        } else{
            if let Some(proto) = self.inner.__proto__{
                return proto.get_by_hash(hash)
            }
        }

        return None
    }

    #[inline]
    pub fn set_property(&mut self, key: JSString, value: Any) {
        
        // the write barriar for object
        if let Some(obj) = value.as_object(){
            self.inner.write_barriar(obj.inner);

        };

        // write barriar for string
        if let Some(s) = value.as_string(){
            self.inner.write_barriar(s.0);
        }

        // write barriar for bigint
        if let Some(b) = value.as_bigint(){
            self.inner.write_barriar(b.as_gc_ptr());
        }

        // calculate the hash key
        let hkey = key.hash();

        self.inner.values.set(&key, value);
    }

    pub fn push(&mut self, value:Any){
        self.inner.values.push(value);
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