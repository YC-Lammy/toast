use inkwell::IntPredicate;
use inkwell::builder::Builder;
use inkwell::values::{FloatValue, IntValue};

use crate::ir_builder::ir::IR;

use super::builder::FunctionBuilder;

pub struct Compiler {}

impl Compiler {
    pub fn is_true<'ctx, B: FunctionBuilder<'ctx>>(
        &mut self,
        builder: &mut B,
        value: FloatValue<'ctx>,
    ) -> IntValue<'ctx> {
        let casted = builder.builder().build_bitcast(value, builder.context().i64_type(), "cast").into_int_value();
        let mask = todo!();

        let masked = builder.builder().build_and(casted, mask, "mask");
        let zero = builder.context().i64_type().const_zero();

        return builder.builder().build_int_compare(IntPredicate::NE, mask, zero, "ne_zero");
    }

    pub fn read_acc<'ctx, B: FunctionBuilder<'ctx>>(
        &mut self,
        builder: &mut B,
    ) -> FloatValue<'ctx> {
        let ptr = builder.acc_ptr();
        builder
            .builder()
            .build_load(builder.context().f64_type(), ptr, "read_acc")
            .into_float_value()
    }

    pub fn write_acc<'ctx, B: FunctionBuilder<'ctx>>(
        &mut self,
        builder: &mut B,
        value: FloatValue<'ctx>,
    ) {
        let ptr = builder.acc_ptr();
        builder.builder().build_store(ptr, value);
    }

    pub fn compile_arithmetic_operation<'ctx, B, F1, F2>(
        &mut self,
        builder: &mut B,
        a: FloatValue<'ctx>,
        b: FloatValue<'ctx>,
        fast: F1,
        slow: F2,
    ) where
        B: FunctionBuilder<'ctx>,
        F1: FnOnce(&mut B, FloatValue<'ctx>, FloatValue<'ctx>) -> FloatValue<'ctx>,
        F2: FnOnce(&mut B, FloatValue<'ctx>, FloatValue<'ctx>) -> FloatValue<'ctx>,
    {
        let fast_block = builder
            .context()
            .append_basic_block(builder.function_value(), "fast_block");
        let slow_block = builder
            .context()
            .append_basic_block(builder.function_value(), "slow_block");
        let exit_block = builder
            .context()
            .append_basic_block(builder.function_value(), "exit_block");

        // check if value is number
        let is_number =
            builder
                .builder()
                .build_float_compare(inkwell::FloatPredicate::ORD, a, b, "ord");

        builder
            .builder()
            .build_conditional_branch(is_number, fast_block, slow_block);

        builder.builder().position_at_end(fast_block);

        {
            let re = fast(builder, a, b);
            self.write_acc(builder, re);

            builder.builder().build_unconditional_branch(exit_block);
        }

        builder.builder().position_at_end(slow_block);

        {
            let re = slow(builder, a, b);
            self.write_acc(builder, re);

            builder.builder().build_unconditional_branch(exit_block);
        }

        builder.builder().position_at_end(exit_block);
    }

    fn compile_until<'ctx, B: FunctionBuilder<'ctx>>(
        &mut self,
        builder: &mut B,
        irs: &[IR],
        cursor: &mut usize,
        until: IR,
    ) {
        while *cursor < irs.len() {
            if irs[*cursor] == until {
                break;
            }

            self.compile_ir(builder, irs, cursor);
        }
    }

    pub fn compile_ir<'ctx, B: FunctionBuilder<'ctx>>(
        &mut self,
        builder: &mut B,
        irs: &[IR],
        cursor: &mut usize,
    ) {
        let index = *cursor;
        *cursor += 1;

        let ir = irs[index];

        match ir {
            IR::Noop => {}
            IR::Debugger => {}

            IR::If => {
                let value = self.read_acc(builder);

                let is_true = self.is_true(builder, value);

                let if_block = builder
                    .context()
                    .append_basic_block(builder.function_value(), "if_true");
                let exit_block = builder
                    .context()
                    .append_basic_block(builder.function_value(), "if_exit");

                builder
                    .builder()
                    .build_conditional_branch(is_true, if_block, exit_block);

                builder.builder().position_at_end(if_block);

                {
                    self.compile_until(builder, irs, cursor, IR::EndIf);

                    builder.builder().build_unconditional_branch(exit_block);
                }

                builder.builder().position_at_end(exit_block);
            }
            IR::IfElse => {
                let value = self.read_acc(builder);

                let is_true = self.is_true(builder, value);

                let if_block = builder
                    .context()
                    .append_basic_block(builder.function_value(), "if_block");
                let else_block = builder
                    .context()
                    .append_basic_block(builder.function_value(), "else_block");
                let exit_block = builder
                    .context()
                    .append_basic_block(builder.function_value(), "if_exit");

                builder
                    .builder()
                    .build_conditional_branch(is_true, if_block, else_block);

                builder.builder().position_at_end(if_block);

                {
                    self.compile_until(builder, irs, cursor, IR::EndIf);

                    builder.builder().build_unconditional_branch(exit_block);
                }

                builder.builder().position_at_end(else_block);

                {
                    self.compile_until(builder, irs, cursor, IR::EndElse);

                    builder.builder().build_unconditional_branch(exit_block);
                }

                builder.builder().position_at_end(exit_block);
            }
            IR::EndIf |
            IR::EndElse => unreachable!(),

            IR::Loop { label } => {
                let loop_body = builder
                    .context()
                    .append_basic_block(builder.function_value(), "loop_body");
                let loop_exit = builder
                    .context()
                    .append_basic_block(builder.function_value(), "loop_exit");

                builder.push_break_block(loop_exit, label.clone());
                builder.push_continue_block(loop_body, label.clone());

                builder.builder().build_unconditional_branch(loop_body);

                {
                    self.compile_until(builder, irs, cursor, IR::EndLoop);

                    builder.builder().build_unconditional_branch(loop_body);
                }

                builder.builder().position_at_end(loop_exit);

                builder.pop_continue_block();
                builder.pop_break_block();
            }
            IR::EndLoop => unreachable!(),

            IR::Block { label } => {
                let exit = builder
                    .context()
                    .append_basic_block(builder.function_value(), "break_exit");

                builder.push_break_block(exit, Some(label.clone()));

                self.compile_until(builder, irs, cursor, IR::EndBlock);

                builder.builder().build_unconditional_branch(exit);

                builder.builder().position_at_end(exit);

                builder.pop_break_block();
            }
            IR::EndBlock => unreachable!(),

            IR::TryCatch => {
                let catch_block = builder
                    .context()
                    .append_basic_block(builder.function_value(), "catch_block");
                let exit_block = builder
                    .context()
                    .append_basic_block(builder.function_value(), "catch_exit");

                builder.push_error_handler(catch_block);

                {
                    self.compile_until(builder, irs, cursor, IR::EndTry);

                    builder.builder().build_unconditional_branch(exit_block);
                }

                builder.pop_error_handler();

                builder.builder().position_at_end(catch_block);

                {
                    let error_ptr = builder.error_ptr();
                    let error = builder
                        .builder()
                        .build_load(builder.context().f64_type(), error_ptr, "load_error")
                        .into_float_value();

                    self.write_acc(builder, error);

                    self.compile_until(builder, irs, cursor, IR::EndCatch);

                    builder.builder().build_unconditional_branch(exit_block);
                }

                builder.builder().position_at_end(exit_block);

                self.compile_until(builder, irs, cursor, IR::EndTryCatchFinalizer);
            }
            IR::EndTry |
            IR::EndCatch |
            IR::EndTryCatchFinalizer => unreachable!(),

            IR::Throw => {
                let value = self.read_acc(builder);
                builder.throw(value);
            }

            IR::Return => {
                let b = builder.return_block();
                builder.builder().build_unconditional_branch(b);

                let dummy = builder.context().append_basic_block(builder.function_value(), "dummy");
                builder.builder().position_at_end(dummy);
            }

            IR::StoreTemp(id) => {
                let value = self.read_acc(builder);
                builder.create_temp(id);
                let ptr = builder.get_temp(id);
                builder.builder().build_store(ptr, value);
            }
            IR::LoadTemp(id) => {
                let ptr = builder.get_temp(id);
                let value = builder
                    .builder()
                    .build_load(builder.context().f64_type(), ptr, "load_temp")
                    .into_float_value();
                self.write_acc(builder, value);
            }
            IR::DropTemp(id) => {
                builder.release_temp(id);
            }

            IR::DeclareVar(id) => {
                builder.declare_variable(id);
            }
            IR::WriteVar(id) => {
                let value = self.read_acc(builder);
                let ptr = builder.get_variable_ptr(id);
                builder.builder().build_store(ptr, value);
            }
            IR::ReadVar(id) => {
                let ptr = builder.get_variable_ptr(id);
                let value = builder
                    .builder()
                    .build_load(builder.context().f64_type(), ptr, "load_var")
                    .into_float_value();
                self.write_acc(builder, value);
            }


            IR::Break { label } => {
                let block = builder.get_break_block(label.as_ref());

                builder.builder().build_unconditional_branch(block);

                let dummy = builder.context().append_basic_block(builder.function_value(), "dummy");
                builder.builder().position_at_end(dummy);
            }
            
        }
    }
}
