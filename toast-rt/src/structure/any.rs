use core::ptr::NonNull;

use super::{object::dynobject::TsDynamicObject, symbol::Symbol};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TsAny(u64);

impl TsAny {
    const TAG_BITS: u64 = 0b1111111111111111 << 48;
    const DATA_BITS: u64 = 0b111111111111111111111111111111111111111111111111;
    const NAN_BITS: u64 = 0b011111111111111 << 48;

    const NAN_TAG: u64 = 0b0111111111111000 << 48;
    /// undefined tag is used for 'undefined' and boolean
    const UNDEFINED_TAG: u64 = 0b0111111111111001 << 48;
    /// data is a pointer to null terminated utf8 string
    const STRING_TAG: u64 = 0b0111111111111010 << 48;
    /// data is a 48bit symbol id
    const SYMBOL_TAG: u64 = 0b0111111111111011 << 48;
    /// data is a 32bit signed integer
    const INT_TAG: u64 = 0b0111111111111100 << 48;
    /// data is a pointer to bigint
    const BIGINT_TAG: u64 = 0b0111111111111101 << 48;
    /// data is a pointer to dynamic object
    const DYNAMIC_OBJECT_TAG: u64 = 0b0111111111111110 << 48;
    /// data is a pointer to static object
    const STATIC_OBJECT_TAG: u64 = 0b0111111111111111 << 48;

    const UNDEFINED_MASK: u64 = Self::UNDEFINED_TAG;
    const TRUE_MASK: u64 = Self::UNDEFINED_TAG | 0b01;
    const FALSE_MASK: u64 = Self::UNDEFINED_TAG | 0b10;

    pub fn is_float(self) -> bool {
        self.0 & Self::NAN_BITS != Self::NAN_BITS
    }

    pub fn is_nan(self) -> bool {
        self.0 & Self::TAG_BITS == Self::NAN_TAG
    }

    pub fn is_undefined(self) -> bool {
        self.0 == Self::UNDEFINED_MASK
    }

    pub fn is_boolean(self) -> bool {
        self.0 == Self::TRUE_MASK || self.0 == Self::FALSE_MASK
    }

    pub fn is_string(self) -> bool {
        self.0 & Self::TAG_BITS == Self::STRING_TAG
    }

    pub fn is_symbol(self) -> bool {
        self.0 & Self::TAG_BITS == Self::SYMBOL_TAG
    }

    pub fn is_int(self) -> bool {
        self.0 & Self::TAG_BITS == Self::INT_TAG
    }

    pub fn is_bigint(self) -> bool {
        self.0 & Self::TAG_BITS == Self::BIGINT_TAG
    }

    pub fn is_dynamic_object(self) -> bool {
        self.0 & Self::TAG_BITS == Self::DYNAMIC_OBJECT_TAG
    }

    pub fn is_static_object(self) -> bool {
        self.0 & Self::TAG_BITS == Self::STATIC_OBJECT_TAG
    }

    pub fn as_float(self) -> Option<f64> {
        if self.is_float() || self.is_nan() {
            return Some(f64::from_bits(self.0));
        }
        None
    }

    pub fn as_bool(self) -> Option<bool> {
        if self.0 == Self::TRUE_MASK {
            return Some(true);
        }
        if self.0 == Self::FALSE_MASK {
            return Some(false);
        }
        None
    }

    pub fn as_string(self) -> Option<NonNull<u8>> {
        if self.is_string() {
            let ptr = (self.0 & Self::DATA_BITS) as *mut u8;
            return Some(NonNull::new(ptr).expect("null pointer"));
        }
        None
    }

    pub fn as_symbol(self) -> Option<Symbol> {
        if self.is_symbol() {
            let id = self.0 & Self::DATA_BITS;
            return Some(Symbol(id));
        }
        None
    }

    pub fn as_int(self) -> Option<i32> {
        if self.is_int() {
            let i = self.0 & Self::DATA_BITS;
            return Some(i as u32 as i32);
        }
        None
    }

    pub fn as_dynamic_object(self) -> Option<NonNull<TsDynamicObject>> {
        if self.is_dynamic_object() {
            let ptr = (self.0 & Self::DATA_BITS) as *mut TsDynamicObject;

            return Some(NonNull::new(ptr).expect("null pointer"));
        }
        None
    }
}
