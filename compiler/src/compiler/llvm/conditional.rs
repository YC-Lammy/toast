use super::*;

impl<'ctx> FunctionBuilder<'ctx> {
    pub fn compile_if(&self, ir: &[IR], cursor: &mut usize) {
        let if_block = self
            .module
            .get_context()
            .append_basic_block(self.function, "if");
        let exit_block = self
            .module
            .get_context()
            .append_basic_block(self.function, "if_exit");

        let value = self.read_acc();
        let comparison = self.is_true(value);

        self.builder
            .build_conditional_branch(comparison, if_block, exit_block);

        // if true
        {
            self.builder.position_at_end(if_block);

            self.compile_until(ir, cursor, IR::EndIf);

            self.builder.build_unconditional_branch(exit_block);
        }

        self.builder.position_at_end(exit_block);
    }

    pub fn compile_if_else(&self, ir: &[IR], cursor: &mut usize) {
        let if_block = self
            .module
            .get_context()
            .append_basic_block(self.function, "if");
        let else_block = self
            .module
            .get_context()
            .append_basic_block(self.function, "if_else");
        let exit_block = self
            .module
            .get_context()
            .append_basic_block(self.function, "if_exit");

        let value = self.read_acc();
        let comparison = self.is_true(value);

        self.builder
            .build_conditional_branch(comparison, if_block, else_block);

        // if true
        {
            // translate if block
            self.builder.position_at_end(if_block);

            self.compile_until(ir, cursor, IR::EndIf);

            // jump to exit
            self.builder.build_unconditional_branch(exit_block);
        }

        // else
        {
            // translate else block
            self.builder.position_at_end(else_block);

            self.compile_until(ir, cursor, IR::EndElse);

            // jump to exit
            self.builder.build_unconditional_branch(exit_block);
        }

        self.builder.position_at_end(exit_block);
    }
}
