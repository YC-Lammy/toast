pub mod llvm;

pub trait Backend{
    fn compile(&mut self, context: &crate::Context) -> Result<ObjectFile, String>;
}

pub struct ObjectFile{

}