
pub mod bigint;
pub mod function;
mod object_map;
pub mod object;
pub mod string;
pub mod symbol;
pub mod regex;
pub mod generator;
pub mod async_function;
pub mod async_generator;

pub use function::*;
pub use object::*;
pub use string::*;
pub use symbol::*;

use self::bigint::Bigint;


pub trait JSValue{
    fn data_bits(&self) -> u64;
    fn type_tag(&self) -> u64;
    fn from_any(any:Any) -> Self;

    #[inline]
    fn to_any(&self) -> Any{
        Any(self.type_tag() | self.data_bits())
    }
}

impl JSValue for f64{
    fn data_bits(&self) -> u64 {
        self.to_bits()
    }

    fn type_tag(&self) -> u64 {
        0
    }

    fn from_any(any:Any) -> Self {
        f64::from_bits(any.0)
    }
}

impl<T:JSValue> From<T> for Any{
    fn from(value: T) -> Self {
        return value.to_any()
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct Any(u64);

impl Any {
    pub const NAN_BITS: u64 = 0b0111111111111000000000000000000000000000000000000000000000000000;
    pub const DATA_BITS: u64 = 0b0000000000000000111111111111111111111111111111111111111111111111;
    pub const TAG_BITS: u64 = 0b1111111111111111000000000000000000000000000000000000000000000000;

    const MASK_BOOL: u64 = 0b0000000000001001000000000000000000000000000000000000000000000000;
    const MASK_INT: u64 = 0b0000000000001010000000000000000000000000000000000000000000000000;
    const MASK_UNDEFINED: u64 = 0b0000000000001011000000000000000000000000000000000000000000000000;
    const MASK_SYMBOL: u64 = 0b0000000000001100000000000000000000000000000000000000000000000000;
    const MASK_BIGINT: u64 = 0b0000000000001101000000000000000000000000000000000000000000000000;
    const MASK_OBJECT: u64 = 0b0000000000001110000000000000000000000000000000000000000000000000;
    const MASK_STRING: u64 = 0b0000000000001111000000000000000000000000000000000000000000000000;
    //const MASK_OBJECT: u64 = 0b1000000000001001000000000000000000000000000000000000000000000000;
    //const MASK_STRING: u64 = 0b1000000000001010000000000000000000000000000000000000000000000000;
    //const MASK_BIGINT: u64 = 0b1000000000001011000000000000000000000000000000000000000000000000;
    //const MASK_1: u64 = 0b1000000000001100000000000000000000000000000000000000000000000000;
    //const MASK_2: u64 = 0b1000000000001101000000000000000000000000000000000000000000000000;
    //const MASK_3: u64 = 0b1000000000001110000000000000000000000000000000000000000000000000;
    //const MASK_4: u64 = 0b1000000000001111000000000000000000000000000000000000000000000000;

    pub const BOOL_TAG: u64 = Self::MASK_BOOL | Self::NAN_BITS;
    pub const UNDEFINED_TAG: u64 = Self::MASK_UNDEFINED | Self::NAN_BITS;
    pub const INT_TAG: u64 = Self::MASK_INT | Self::NAN_BITS;
    //pub const BIGINT32_TAG: u64 = Self::MASK_BIGINT32 | Self::NAN_BITS;
    pub const SYMBOL_TAG: u64 = Self::MASK_SYMBOL | Self::NAN_BITS;
    pub const OBJECT_TAG: u64 = Self::MASK_OBJECT | Self::NAN_BITS;
    pub const STRING_TAG: u64 = Self::MASK_STRING | Self::NAN_BITS;
    pub const BIGINT_TAG: u64 = Self::MASK_BIGINT | Self::NAN_BITS;

    pub const FALSE: Self = Self(Self::BOOL_TAG);
    pub const TRUE: Self = Self(Self::BOOL_TAG | 0x1);
    /// null is an unallocated object
    pub const NULL: Self = Self(Self::OBJECT_TAG);
    pub const UNDEFINED: Self = Self(Self::UNDEFINED_TAG);
    pub const NAN: Self = Self(Self::NAN_BITS);

    #[inline]
    pub fn data(&self) -> u64 {
        self.0 & Self::DATA_BITS
    }

    pub fn tag(&self) -> u64 {
        self.0 & Self::TAG_BITS
    }

    pub fn is_nan(&self) -> bool {
        self.0 == Self::NAN.0
    }

    pub fn is_number(&self) -> bool {
        self.0 & Self::NAN_BITS != Self::NAN_BITS || self.0 == Self::NAN_BITS
    }

    pub fn is_bool(&self) -> bool {
        (self.0 & Self::TAG_BITS) == Self::BOOL_TAG
    }

    pub fn is_null(&self) -> bool {
        self.0 == Self::OBJECT_TAG
    }

    pub fn is_undefined(&self) -> bool {
        self.0 == Self::UNDEFINED_TAG
    }

    pub fn is_int(&self) -> bool {
        self.0 & Self::INT_TAG == Self::INT_TAG
    }

    pub fn is_symbol(&self) -> bool {
        (self.0 & Self::TAG_BITS) == Self::SYMBOL_TAG
    }

    pub fn is_object(&self) -> bool {
        (self.0 & Self::TAG_BITS) == Self::OBJECT_TAG
    }

    pub fn is_bigint(&self) -> bool {
        (self.0 >> 48) == (Self::BIGINT_TAG >> 48)
    }

    pub fn is_string(&self) -> bool {
        (self.0 >> 48) == (Self::STRING_TAG >> 48)
    }
    
    pub fn as_number(&self) -> Option<f64> {
        if self.is_number() {
            return Some(f64::from_bits(self.0));
        }
        return None;
    }

    pub fn as_bigint(&self) -> Option<Bigint> {
        if self.is_bigint() {
            return Some(Bigint::from_any(*self));
        }
        return None;
    }

    pub fn as_string(&self) -> Option<JSString> {
        if self.is_string() {
            return Some(JSString::from_any(*self));
        }
        return None;
    }

    pub fn as_object(&self) -> Option<&'static mut Object> {
        if self.is_object() && !self.is_null(){
            return Some(unsafe { (self.data() as *mut Object).as_mut().unwrap() });
        }
        return None;
    }

    pub fn as_symbol(&self) -> Option<JSSymbol> {
        if self.is_symbol() {
            return Some(JSSymbol(self.data()));
        }
        return None;
    }

    pub fn to_string(self) -> JSString {
        // undefined
        if self.is_undefined() {
            return JSString::new("undefined");
        }

        // boolean
        if self == Self::FALSE {
            return JSString::new("false");
        }
        if self == Self::TRUE {
            return JSString::new("true");
        }

        // string
        if let Some(s) = self.as_string() {
            return s;
        }

        // number
        if let Some(n) = self.as_number() {
            let mut buf = native_js_common::ftoa::Buffer::new();
            let s = buf.format(n);

            return JSString::from_str(s);
        }

        // bigint
        if let Some(b) = self.as_bigint() {
            return b.to_string();
        }

        // symbol
        if let Some(s) = self.as_symbol() {
            return s.as_str();
        }

        // null
        if self.is_null() {
            return JSString::new("null");
        }

        if let Some(obj) = self.as_object() {
            if let Some(v) = obj.get_by_hash(native_js_common::hash_string("toString")) {
                return v.call(Any::UNDEFINED, &[]).to_string()
            } else {
                return JSString::new("[object Object]");
            }
        }

        unreachable!()
    }

    pub fn to_bool(self) -> bool{
        if self == Self::TRUE{
            return true
        }

        if self == Self::FALSE{
            return false
        }

        if self.is_symbol(){
            return true
        }

        if let Some(i) = self.as_number(){
            return i != 0.0
        }

        if let Some(obj) = self.as_object(){
            return true
        }

        if let Some(s) = self.as_string(){
            return s.len() != 0
        }

        if let Some(b) = self.as_bigint(){
            return b.value() != 0
        }

        return false;
    }
    

    pub fn error(msg:&str) -> Any{
        todo!()
    }
}

impl Any{
    pub fn get_property(self, key:JSString) -> Option<Any>{
        if let Some(obj) = self.as_object(){
            return obj.get_property(key)
        }
        todo!()
    }
    
    pub fn call(self, this:Any, args:&[Any]) -> Any{
        if let Some(obj) = self.as_object(){
            return obj.call(this, args)
        } else{
            crate::unwinding::throw(Any::error("Cannot call on non function object"));
        }
    }
}

impl iron_gc::Trace for Any{
    fn trace(&mut self, visitor: &mut iron_gc::Visitor) {
        if let Some(obj) = self.as_object(){
            visitor.visit(obj.inner);
        }

        if let Some(big) = self.as_bigint(){
            visitor.visit(big.0)
        }

        if let Some(mut s) = self.as_string(){
            s.trace(visitor);
        }
    }
}

impl PartialEq for Any{
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[test]
fn test_validate_any(){
    assert!(993.32487f64.to_any().as_number() == Some(993.32487f64));
    assert!(JSString::new("hello world").to_any().as_string() == Some(JSString::new("hello world")));
}