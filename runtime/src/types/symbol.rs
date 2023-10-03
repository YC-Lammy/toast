
use alloc::vec::Vec;
use parking_lot::RwLock;

use rusty_ts_macro::{tssting, hash};

use crate::utils::GcHashMap;

use super::JSString;

lazy_static::lazy_static! {
    static ref SYMBOL_STRINGS:RwLock<Vec<(u64, JSString)>> = {
        let builtins = &[
            "@@asyncIterator",
            "@@hasInstance",
            "@@isConcatSpreadable",
            "@@iterator",
            "@@match",
            "@@matchAll",
            "@@replace",
            "@@search",
            "@@species",
            "@@split",
            "@@toPrimitive",
            "@@toStringTag",
            "@@unscopables"
        ];

        let mut v = Vec::new();
        RwLock::new(v)
    };
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct JSSymbol(pub u64);

impl JSSymbol {
    pub const ASYN_ITERATOR: Self = Self(hash!("@@asyncIterator"));

    pub fn as_str(self) -> JSString {
        let syms = SYMBOL_STRINGS.read();

        for (h, sym) in syms.iter(){
            if *h == self.0{
                return *sym
            }
        };

        unreachable!()
    }
}