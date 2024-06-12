

use parking_lot::RwLock;

use super::string::TsString;

lazy_static::lazy_static!{
    static ref GLOBAL_SYMBOL_REGISTRY: RwLock<Vec<(Symbol, TsString)>> = RwLock::new(vec![]);
}


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Symbol(pub(crate) u64);

impl Symbol{
    pub fn to_string(&self) -> TsString{
        let glob = GLOBAL_SYMBOL_REGISTRY.read();

        let idx = glob.binary_search_by(|(s, _)|s.cmp(self)).expect("invalid symbol");

        return glob[idx].1.clone()
    }
}