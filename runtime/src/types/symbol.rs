
use alloc::vec::Vec;
use parking_lot::RwLock;

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

        let mut v = Vec::with_capacity(builtins.len());

        for b in builtins{
            let s = JSString::new(b);
            s.0.set_uncollectable();

            v.push((s.hash(), s));
        }

        RwLock::new(v)
    };
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct JSSymbol(pub u64);

impl JSSymbol {
    
    pub fn as_str(self) -> JSString {
        let syms = SYMBOL_STRINGS.read();

        for (h, sym) in syms.iter(){
            if *h == self.0{
                return *sym
            }
        };

        return JSString::new("@@unknown")
    }
}