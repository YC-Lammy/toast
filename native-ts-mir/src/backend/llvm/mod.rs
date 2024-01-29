use std::collections::HashMap;





pub struct LLVMBackend{
    context: llvm_sys::LLVMContext,
}

impl super::Backend for LLVMBackend{
    fn compile(&mut self, context: &crate::Context) -> Result<super::ObjectFile, String> {
        let module = llvm_sys::core::LLVMModuleCreateWithNameInContext("", &self.context);
        let mut builder = LLVMBuilder{
            ctx: module.get_context(),
            module,
            builder: self.context.create_builder(),

            functions: Vec::new(),
        };

        builder.compile(context)?;

        return Ok(todo!())
    }
}

struct LLVMBuilder<'ctx>{
    ctx: inkwell::context::ContextRef<'ctx>,
    module: inkwell::module::Module<'ctx>,
    builder: inkwell::builder::Builder<'ctx>,

    functions: Vec<inkwell::values::FunctionValue<'ctx>>
}

impl<'ctx> LLVMBuilder<'ctx>{
    pub fn compile(&mut self, context: &crate::Context) -> Result<(), String>{
        for (id, func) in context.functions.iter().enumerate(){
            let return_ty = self.translate_type(&func.ty.return_);
            let mut params = Vec::new();
            for t in func.ty.params.iter(){
                params.push(self.translate_type(t));
            }

            let fn_ty = self.translate_function_type(return_ty, &params);

            let f = self.module.add_function(
                func.name.as_ref().map(|n|{n.as_str()}).unwrap_or(itoa::Buffer::new().format(id)), 
                fn_ty, 
                match func.linkage{
                    Some(crate::Linkage::Appending) => Some(inkwell::module::Linkage::Appending),
                    Some(crate::Linkage::AvailableExternally) => Some(inkwell::module::Linkage::AvailableExternally),
                    Some(crate::Linkage::Common) => Some(inkwell::module::Linkage::Common),
                    Some(crate::Linkage::DLLExport) => Some(inkwell::module::Linkage::DLLExport),
                    Some(crate::Linkage::DLLImport) => Some(inkwell::module::Linkage::DLLImport),
                    Some(crate::Linkage::ExternWeak) => Some(inkwell::module::Linkage::ExternalWeak),
                    Some(crate::Linkage::External) => Some(inkwell::module::Linkage::External),
                    Some(crate::Linkage::Internal) => Some(inkwell::module::Linkage::Internal),
                    Some(crate::Linkage::LinkOnce) => Some(inkwell::module::Linkage::LinkOnceAny),
                    Some(crate::Linkage::LinkOnceOdr) => Some(inkwell::module::Linkage::LinkOnceODR),
                    Some(crate::Linkage::Private) => Some(inkwell::module::Linkage::Private),
                    Some(crate::Linkage::Weak) => Some(inkwell::module::Linkage::WeakAny),
                    Some(crate::Linkage::WeakOdr) => Some(inkwell::module::Linkage::WeakODR),
                    None => None
                }
            );

            self.functions.push(f);
        };


        return Ok(())
    }

    pub fn translate_type(&self, ty: &crate::Type) -> inkwell::types::AnyTypeEnum<'ctx>{
        match ty{
            crate::Type::Aggregate(a) => {

            },
            crate::Type::Array(a) => {
                self.translate_type(ty)
            }
        }
    }

    pub fn translate_function_type(&self, return_ty: inkwell::types::AnyTypeEnum<'ctx>, params: &[inkwell::types::AnyTypeEnum<'ctx>]) -> inkwell::types::FunctionType<'ctx>{

    }
}