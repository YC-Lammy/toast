use super::*;

impl<'ctx> FunctionBuilder<'ctx> {
    pub fn compile_create_object(&self) {
        let ct_obj = self.module.get_function("RT_create_object").unwrap();
        let re = self.builder.build_call(ct_obj, &[], "create_object");

        self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());
    }

    pub fn compile_create_array(&self, size: usize) {
        // fn(u32) -> Any
        let ct_array = self.module.get_function("RT_create_array").unwrap();
        let re = self.builder.build_direct_call(
            ct_array,
            &[self.context.i32_type().const_int(size as u64, false).into()],
            "create_array",
        );

        self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());
    }

    pub fn compile_array_push(&self, array: TempId) {
        let array = self.read_temp(array);
        let value = self.read_acc();

        // fn (array:Any, value:Any)
        let array_push = self.module.get_function("RT_array_push").unwrap();
        self.builder
            .build_direct_call(array_push, &[array.into(), value.into()], "rt_array_push");
    }
}
