use native_ts_mir::types::Interface;
use native_ts_mir::types::{
    Type
};

use native_ts_hir::ast::Module;


pub struct Translator{
    context: native_ts_mir::Context,
}

impl Translator{
    pub fn new() -> Self{
        let ctx = native_ts_mir::Context::new();
        Self { context: ctx }
    }

    pub fn translate(&self, module: &Module){
        
    }
}

pub struct TranslatorBuilder<'ctx>{
    ctx: &'ctx native_ts_mir::Context,
    any_type: Interface<'ctx>,
}