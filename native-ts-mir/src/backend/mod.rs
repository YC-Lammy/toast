//pub mod llvm;

pub trait Backend {
    type Output;
    fn compile(&mut self, context: &crate::Context) -> Result<Self::Output, String>;
}

pub struct ObjectFile {}
