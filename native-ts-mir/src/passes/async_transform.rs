use crate::{Context, Function, runtime::{Runtime, AsyncRuntime}, mir::MIR};




pub struct AsyncTransform;

impl AsyncTransform{
    pub fn run(&self, ctx: &mut Context, runtime: &Runtime){
        for f in &mut ctx.functions{
            if let Some(f) = &mut f.function{
                if f.is_async && f.is_generator.is_none(){
                    self.transform_function(f, runtime.async_runtime.as_ref().expect("missing async runtime"));
                }
                
            }
        }
    }

    fn transform_function(&self, func: &mut Function, runtime: &AsyncRuntime){
        let entry = &mut func.blocks[0];

        /*
        entry.inst.push(MIR::Call { 
            id: (), 
            args: (), 
            return_: ()
        });*/

        for block in &mut func.blocks{
            for inst in &mut block.inst{
                if let MIR::AsyncAwait(future, result) = inst{
                    
                }
            }
        }
    }
}