

use iron_gc::GcPtr;

use crate::asynchronous::Promise;

use super::*;
use super::object_map::ObjectMap;
use super::regex::Regexp;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum ObjectInternal {
    Empty,
    Function,
    FunctionBind,
    Closure,
    Generator,
    RegEx,
    Promise,

    String,
    Symbol,
    Boolean,
    Number,
    BigInt,
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct Object{
    pub(crate) inner: GcPtr<ObjectInner>
}

#[repr(C)]
pub struct ObjectInner{
    /// internal value
    internal: ObjectInternal,

    __proto__: Option<Object>,

    values: ObjectMap,
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
                internal: ObjectInternal::Empty, 
                __proto__: None,
                values: ObjectMap::new()
            }
        );
        
        return Self{ inner: p }
    }

    pub fn new_promise(promise: Promise) -> Self{
        let p = GcPtr::new(
            ObjectInner { 
                internal: ObjectInternal::Promise, 
                __proto__: None,
                values: ObjectMap::new()
            }
        );

        unsafe{
            let ptr = p.as_ptr().add(1) as *mut Promise;
            ptr.write(promise);
        }
        
        return Self{ inner: p }
    }

    pub fn new_regex(reg: Regexp) -> Self{
        let p = GcPtr::new(
            ObjectInner { 
                internal: ObjectInternal::RegEx, 
                __proto__: None,
                values: ObjectMap::new()
            }
        );

        unsafe{
            let ptr = p.as_ptr().add(1) as *mut Regexp;
            ptr.write(reg);
        }
        
        return Self{ inner: p }
    }
    

    #[inline]
    pub fn get_property(&mut self, key: JSString) -> Option<Any> {

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
            if let Some(proto) = &mut self.inner.__proto__{
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
            s.write_barriar(self.inner);
        }

        // write barriar for bigint
        if let Some(b) = value.as_bigint(){
            self.inner.write_barriar(b.as_gc_ptr());
        }

        self.inner.values.set(&key, value);
    }

    pub fn push(&mut self, value:Any){
        self.inner.values.push(value);
    }

    #[inline]
    pub fn call(&mut self, this:Any, args:&[Any]) -> Any{
        if let Some(func) = self.inner.as_function(){
            return func.dynamic_call(this, 0 as _, args.len() as u32, args.as_ptr())
        }

        if let Some(func) = self.inner.as_function_bind(){
            return func.call(args)
        }

        if let Some(cl) = self.inner.as_closure(){
            return cl.call(this, args)
        }

        crate::unwinding::throw(Any::error("cannot call on non function object"))
    }
}

unsafe impl Sync for ObjectInner{}

impl iron_gc::Trace for ObjectInner{
    fn additional_bytes(&self) -> usize {
        use core::mem::size_of;

        match self.internal{
            ObjectInternal::Empty => 0,
            ObjectInternal::Boolean => size_of::<bool>(),
            ObjectInternal::Closure => size_of::<Closure>(),
            ObjectInternal::FunctionBind => size_of::<FunctionBind>(),
            ObjectInternal::Function => size_of::<JSFunc>(),
            ObjectInternal::Generator => todo!(),
            ObjectInternal::Number => size_of::<f64>(),
            ObjectInternal::Promise => size_of::<Promise>(),
            ObjectInternal::RegEx => size_of::<Regexp>(),
            ObjectInternal::String => size_of::<JSString>(),
            ObjectInternal::Symbol => size_of::<JSSymbol>(),
            ObjectInternal::BigInt => size_of::<u128>()
        }
    }

    fn trace(&mut self, visitor: &mut iron_gc::Visitor) {
        self.values.trace(visitor);

        if let Some(proto) = &mut self.__proto__{
            proto.inner.trace(visitor);
        }

        if let Some(cl) = self.as_closure(){
            cl.trace(visitor);
            return;
        }

        if let Some(func) = self.as_function_bind(){
            func.trace(visitor);
            return;
        }

        if let Some(_gen) = self.as_generator(){
            todo!()
        }

        if let Some(p) = self.as_promise(){
            p.trace(visitor);
            return;
        }

        if let Some(_reg) = self.as_regex(){
            
        }

        if let Some(s) = self.as_string(){
            s.trace(visitor);
            return;
        }
    }
}

impl ObjectInner{
    pub fn as_function(&mut self) -> Option<&mut JSFunc>{
        if self.internal == ObjectInternal::Function{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut JSFunc;
                return ptr.as_mut()
            }
        }
        return None
    }

    pub fn as_function_bind(&mut self) -> Option<&mut FunctionBind>{
        if self.internal == ObjectInternal::FunctionBind{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut FunctionBind;
                return ptr.as_mut()
            }
        }
        return None
    }

    pub fn as_closure(&mut self) -> Option<&mut Closure>{
        if self.internal == ObjectInternal::Closure{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut Closure;
                return ptr.as_mut()
            }
        }
        return None
    }

    pub fn as_generator(&mut self) -> Option<&mut ()>{
        if self.internal == ObjectInternal::Generator{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut ();
                return ptr.as_mut()
            }
        }
        return None
    }

    pub fn as_regex(&mut self) -> Option<&mut Regexp>{
        if self.internal == ObjectInternal::RegEx{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut Regexp;
                return ptr.as_mut()
            }
        }
        return None
    }

    #[inline]
    pub fn as_promise(&mut self) -> Option<&mut Promise>{
        if self.internal == ObjectInternal::Promise{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut Promise;
                return ptr.as_mut()
            }
        }
        return None
    }

    pub fn as_string(&mut self) -> Option<&mut JSString>{
        if self.internal == ObjectInternal::String{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut JSString;
                return ptr.as_mut()
            }
        }
        return None
    }

    pub fn as_symbol(&mut self) -> Option<&mut JSSymbol>{
        if self.internal == ObjectInternal::Symbol{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut JSSymbol;
                return ptr.as_mut()
            }
        }
        return None
    }

    pub fn as_bool(&mut self) -> Option<&mut bool>{
        if self.internal == ObjectInternal::Boolean{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut bool;
                return ptr.as_mut()
            }
        }
        return None
    }

    pub fn as_number(&mut self) -> Option<&mut f64>{
        if self.internal == ObjectInternal::Function{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut f64;
                return ptr.as_mut()
            }
        }
        return None
    }

    pub fn as_bigint(&mut self) -> Option<&mut i128>{
        if self.internal == ObjectInternal::Function{
            unsafe{
                let ptr = (self as *mut Self).add(1) as *mut i128;
                return ptr.as_mut()
            }
        }
        return None
    }
}

#[test]
fn test_obj(){
    panic!("{}", core::mem::size_of::<ObjectInner>())
}