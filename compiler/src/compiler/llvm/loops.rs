use crate::ir_builder::ir::IterId;

use super::*;

impl<'ctx> FunctionBuilder<'ctx> {
    pub fn push_loop(&self, body: BasicBlock<'ctx>, exit: BasicBlock<'ctx>, label: Option<JsWord>) {
        self.continues.borrow_mut().push((body, label.clone()));
        self.breaks.borrow_mut().push((exit, label));
    }

    pub fn pop_loop(&self) {
        self.continues.borrow_mut().pop();
        self.breaks.borrow_mut().pop();
    }

    fn find_break_with_label(&self, label: &JsWord) -> BasicBlock<'ctx> {
        
        for (exit, l) in self.breaks.borrow().iter(){
            if let Some(l) = l{
                if l.eq(label){
                    return *exit
                }
            }
        }
        unreachable!()
    }

    fn get_last_break(&self) -> BasicBlock<'ctx>{
        return self.breaks.borrow().last().unwrap().0;
    }

    fn find_continue_with_label(&self, label: &JsWord) -> BasicBlock<'ctx> {
        
        for (exit, l) in self.continues.borrow().iter(){
            if let Some(l) = l{
                if l.eq(label){
                    return *exit
                }
            }
        }
        unreachable!()
    }

    fn get_last_continue(&self) -> BasicBlock<'ctx>{
        return self.continues.borrow().last().unwrap().0;
    }

    pub fn create_for_in_iter(&self, id: IterId) {
        let value = self.read_acc();
        // fn(Any) -> *mut iter
        let c = self.module.get_function("RT_create_for_in_iter").unwrap();

        let re = self
            .builder
            .build_call(c, &[value.into()], "create_for_in_iter");
        let iter = re.try_as_basic_value().left().unwrap().into_pointer_value();

        self.iterators.borrow_mut().insert(id, iter);
    }

    pub fn create_for_of_iter(&self, id: IterId) {
        let value = self.read_acc();
        // fn(Any) -> *mut iter
        let c = self.module.get_function("RT_create_for_of_iter").unwrap();

        let re = self
            .builder
            .build_call(c, &[value.into()], "create_for_of_iter");
        let iter = re.try_as_basic_value().left().unwrap().into_pointer_value();

        self.iterators.borrow_mut().insert(id, iter);
    }

    pub fn create_async_iter(&self, id: IterId) {
        let value = self.read_acc();
        // fn(Any) -> *mut iter
        let c = self.module.get_function("RT_create_async_iter").unwrap();

        let re = self
            .builder
            .build_call(c, &[value.into()], "create_async_iter");
        let iter = re.try_as_basic_value().left().unwrap().into_pointer_value();

        self.iterators.borrow_mut().insert(id, iter);
    }

    pub fn iter_next(&self, id: IterId) {
        let iter = *self.iterators.borrow().get(&id).unwrap();

        // fn(*mut iter) -> Any
        let iter_next = self.module.get_function("RT_iter_next").unwrap();

        let re = self
            .builder
            .build_call(iter_next, &[iter.into()], "iter_next");
        self.write_acc(re.try_as_basic_value().left().unwrap().into_float_value());
    }

    pub fn is_iter_done(&self, id: IterId) -> IntValue<'ctx> {
        let iter = *self.iterators.borrow().get(&id).unwrap();

        // fn(*mut iter) -> bool
        let is_iter_done = self.module.get_function("RT_iter_done").unwrap();
        let re = self
            .builder
            .build_call(is_iter_done, &[iter.into()], "iter_done");
        return re.try_as_basic_value().left().unwrap().into_int_value();
    }

    pub fn drop_iter(&self, id: IterId) {
        let iter = self.iterators.borrow_mut().remove(&id).unwrap();
        let drop_iter = self.module.get_function("RT_drop_iter").unwrap();
        self.builder
            .build_call(drop_iter, &[iter.into()], "drop_iter");
    }

    pub fn compile_continue(&self, label: &Option<JsWord>) {
        let body = if let Some(label) = label {
            self.find_continue_with_label(label)
        } else {
            self.get_last_continue()
        };

        self.builder.build_unconditional_branch(body);
    }

    pub fn compile_break_if_iter_done(&self, id: IterId) {
        let loop_exit = self.get_last_break();

        let continue_block = self
            .module
            .get_context()
            .append_basic_block(self.function, "exit_block");

        let is_iter_done = self.is_iter_done(id);

        // jump to exit if iter done
        self.builder
            .build_conditional_branch(is_iter_done, loop_exit, continue_block);

        self.builder.position_at_end(continue_block);
    }

    pub fn compile_break_if_false(&self) {
        let value = self.read_acc();
        let is_true = self.is_true(value);

        let loop_exit = self.get_last_break();

        let continue_block = self
            .module
            .get_context()
            .append_basic_block(self.function, "exit_block");

        // continue if is true
        self.builder
            .build_conditional_branch(is_true, continue_block, loop_exit);

        self.builder.position_at_end(continue_block);
    }

    pub fn compile_break(&self, label: &Option<JsWord>) {
        let exit = if let Some(label) = label {
            self.find_break_with_label(label)
        } else {
            self.get_last_break()
        };

        let dummy_block = self
            .context
            .append_basic_block(self.function, "dummy_block");
        // branch to exit
        self.builder.build_unconditional_branch(exit);

        self.builder.position_at_end(dummy_block);
    }

    pub fn compile_loop(&self, ir: &[IR], cursor: &mut usize, label: &Option<JsWord>) {
        let loop_body = self.context.append_basic_block(self.function, "loop_body");
        let loop_station = self
            .context
            .append_basic_block(self.function, "loop_station");
        let loop_exit = self.context.append_basic_block(self.function, "loop_exit");

        self.push_loop(loop_body, loop_exit, label.clone());

        self.builder.build_unconditional_branch(loop_body);

        self.builder.position_at_end(loop_body);

        self.compile_until(ir, cursor, IR::EndLoop);

        // goto next loop
        self.builder.build_unconditional_branch(loop_station);

        self.builder.position_at_end(loop_station);
        self.builder.build_unconditional_branch(loop_body);

        self.builder.position_at_end(loop_exit);

        self.pop_loop();
    }
}
