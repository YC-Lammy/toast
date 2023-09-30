
use parking_lot::RwLock;

use rusty_ts_macro::{tssting, hash};

use crate::utils::GcHashMap;

use super::JSString;

lazy_static::lazy_static! {
    static ref SYMBOL_STRINGS:RwLock<GcHashMap<u64, JSString>> = {
        let mut m = GcHashMap::default();
        let builtins = &[
            (hash!("@@asyncIterator"), JSString::new("@@asyncIterator")),
            (hash!("@@hasInstance"), JSString::new("@@hasInstance")),
            (hash!("@@isConcatSpreadable"), JSString::new("@@isConcatSpreadable")),
            (hash!("@@iterator"), JSString::new("@@iterator")),
            (hash!("@@match"), JSString::new("@@match")),
            (hash!("@@matchAll"), JSString::new("@@matchAll")),
            (hash!("@@replace"), JSString::new("@@replace")),
            (hash!("@@search"), JSString::new("@@search")),
            (hash!("@@species"), JSString::new("@@species")),
            (hash!("@@split"), JSString::new("@@split")),
            (hash!("@@toPrimitive"), JSString::new("@@toPrimitive")),
            (hash!("@@toStringTag"), JSString::new("@@toStringTag")),
            (hash!("@@unscopables"), JSString::new("@@unscopables"))
        ];
        for (i, s) in builtins{
            m.insert(*i, *s);
        }
        RwLock::new(m)
    };
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct JSSymbol(pub u64);

impl JSSymbol {
    pub const ASYN_ITERATOR: Self = Self(hash!("@@asyncIterator"));

    pub fn as_str(self) -> JSString {
        *SYMBOL_STRINGS.read().get(&self.0).unwrap()
    }
}

#[test]
fn test_builtin_symbols_correct() {

    let builtins = &[
        (4929908495512011759u64, "@@asyncIterator"),
        (7096794315332740597, "@@hasInstance"),
        (1903513332595665656, "@@isConcatSpreadable"),
        (4808481507657009854, "@@iterator"),
        (10812773187193793362, "@@match"),
        (17754630021141553842, "@@matchAll"),
        (14304821779148366697, "@@replace"),
        (12662756130399689106, "@@search"),
        (13758924030077575543, "@@species"),
        (15253022237696633049, "@@split"),
        (12851622145040706162, "@@toPrimitive"),
        (12617633200751318853, "@@toStringTag"),
        (17627178712090155137, "@@unscopables"),
    ];
    for (i, s) in builtins {
        let h:u64 = cityhasher::hash(s.as_bytes());
        debug_assert!(*i == h, "hash of {} should be {}, found {}.", s, *i, h)
    }
}
