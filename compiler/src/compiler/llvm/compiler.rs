use inkwell::builder::Builder;
use inkwell::values::{FloatValue, IntValue};
use inkwell::IntPredicate;

use crate::ir_builder::ir::{TempId, IR};

use super::builder::FunctionBuilder;

pub struct Compiler {}

impl Compiler {
    const PTR_MASK: u64 = 0b0000000000000000111111111111111111111111111111111111111111111111;

    pub const NAN_BITS: u64 = 0b0111111111111000000000000000000000000000000000000000000000000000;
    pub const DATA_BITS: u64 = 0b0000000000000000111111111111111111111111111111111111111111111111;
    pub const TAG_BITS: u64 = 0b1111111111111111000000000000000000000000000000000000000000000000;

    const MASK_BOOL: u64 = 0b0000000000001001000000000000000000000000000000000000000000000000;
    const MASK_INT: u64 = 0b0000000000001010000000000000000000000000000000000000000000000000;
    const MASK_UNDEFINED: u64 = 0b0000000000001011000000000000000000000000000000000000000000000000;
    const MASK_SYMBOL: u64 = 0b0000000000001100000000000000000000000000000000000000000000000000;
    const MASK_BIGINT: u64 = 0b0000000000001101000000000000000000000000000000000000000000000000;
    const MASK_OBJECT: u64 = 0b0000000000001110000000000000000000000000000000000000000000000000;
    const MASK_STRING: u64 = 0b0000000000001111000000000000000000000000000000000000000000000000;
    //const MASK_OBJECT: u64 = 0b1000000000001001000000000000000000000000000000000000000000000000;
    //const MASK_STRING: u64 = 0b1000000000001010000000000000000000000000000000000000000000000000;
    //const MASK_BIGINT: u64 = 0b1000000000001011000000000000000000000000000000000000000000000000;
    //const MASK_1: u64 = 0b1000000000001100000000000000000000000000000000000000000000000000;
    //const MASK_2: u64 = 0b1000000000001101000000000000000000000000000000000000000000000000;
    //const MASK_3: u64 = 0b1000000000001110000000000000000000000000000000000000000000000000;
    //const MASK_4: u64 = 0b1000000000001111000000000000000000000000000000000000000000000000;

    pub const BOOL_TAG: u64 = Self::MASK_BOOL | Self::NAN_BITS;
    pub const UNDEFINED_TAG: u64 = Self::MASK_UNDEFINED | Self::NAN_BITS;
    pub const INT_TAG: u64 = Self::MASK_INT | Self::NAN_BITS;
    //pub const BIGINT32_TAG: u64 = Self::MASK_BIGINT32 | Self::NAN_BITS;
    pub const SYMBOL_TAG: u64 = Self::MASK_SYMBOL | Self::NAN_BITS;
    pub const OBJECT_TAG: u64 = Self::MASK_OBJECT | Self::NAN_BITS;
    pub const STRING_TAG: u64 = Self::MASK_STRING | Self::NAN_BITS;
    pub const BIGINT_TAG: u64 = Self::MASK_BIGINT | Self::NAN_BITS;

    pub const FALSE: u64 = Self::BOOL_TAG;
    pub const TRUE: u64 = Self::BOOL_TAG | 0x1;
    /// null is an unallocated object
    pub const NULL: u64 = Self::OBJECT_TAG;
    pub const UNDEFINED: u64 = Self::UNDEFINED_TAG;
    pub const NAN: u64 = Self::NAN_BITS;

    pub fn is_true<'ctx, B: FunctionBuilder<'ctx>>(
        &mut self,
        builder: &mut B,
        value: FloatValue<'ctx>,
    ) -> IntValue<'ctx> {
        let casted = builder
            .builder()
            .build_bitcast(value, builder.context().i64_type(), "cast")
            .into_int_value();

        let mask = builder
            .context()
            .i64_type()
            .const_int(Self::PTR_MASK, false);

        let masked = builder.builder().build_and(casted, mask, "mask");
        let zero = builder.context().i64_type().const_zero();

        return builder
            .builder()
            .build_int_compare(IntPredicate::NE, masked, zero, "ne_zero");
    }

    pub fn is_false<'ctx, B: FunctionBuilder<'ctx>>(
        &mut self,
        builder: &mut B,
        value: FloatValue<'ctx>,
    ) -> IntValue<'ctx> {
        let casted = builder
            .builder()
            .build_bitcast(value, builder.context().i64_type(), "cast")
            .into_int_value();

        let mask = builder
            .context()
            .i64_type()
            .const_int(Self::PTR_MASK, false);

        let masked = builder.builder().build_and(casted, mask, "mask");
        let zero = builder.context().i64_type().const_zero();

        return builder
            .builder()
            .build_int_compare(IntPredicate::EQ, masked, zero, "eq_zero");
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

    pub fn read_temp<'ctx, B: FunctionBuilder<'ctx>>(
        &mut self,
        builder: &mut B,
        id: TempId,
    ) -> FloatValue<'ctx> {
        let ptr = builder.get_temp(id);
        builder
            .builder()
            .build_load(builder.context().f64_type(), ptr, "read_temp")
            .into_float_value()
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
            IR::EndIf | IR::EndElse => unreachable!(),

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
            IR::EndTry | IR::EndCatch | IR::EndTryCatchFinalizer => unreachable!(),

            IR::Throw => {
                let value = self.read_acc(builder);
                builder.throw(value);
            }

            IR::Return => {
                let b = builder.return_block();
                builder.builder().build_unconditional_branch(b);

                let dummy = builder
                    .context()
                    .append_basic_block(builder.function_value(), "dummy");
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

                let dummy = builder
                    .context()
                    .append_basic_block(builder.function_value(), "dummy");
                builder.builder().position_at_end(dummy);
            }

            IR::BreakIfFalse => {
                let value = self.read_acc(builder);

                let is_false = self.is_false(builder, value);

                let continue_block = builder
                    .context()
                    .append_basic_block(builder.function_value(), "not_break");

                let break_block = builder.get_break_block(None);

                builder
                    .builder()
                    .build_conditional_branch(is_false, break_block, continue_block);

                builder.builder().position_at_end(continue_block);
            }
            IR::BreakIfIterDone(iter) => {
                let iter = builder.get_iterator(iter);
                let func = builder
                    .module()
                    .get_function("__netivejs_iter_done")
                    .unwrap();

                let is_done = builder
                    .builder()
                    .build_call(func, &[iter.into()], "iter_done")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();

                let continue_block = builder
                    .context()
                    .append_basic_block(builder.function_value(), "not_break");

                let break_block = builder.get_break_block(None);

                builder
                    .builder()
                    .build_conditional_branch(is_done, break_block, continue_block);

                builder.builder().position_at_end(continue_block);
            }
            IR::Continue { label } => {
                let block = builder.get_continue_block(label.as_ref());

                builder.builder().build_unconditional_branch(block);

                let dummy = builder
                    .context()
                    .append_basic_block(builder.function_value(), "dummy");
                builder.builder().position_at_end(dummy);
            }

            IR::Add(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                self.compile_arithmetic_operation(
                    builder,
                    a,
                    b,
                    |builder, a, b| builder.builder().build_float_add(a, b, "fadd"),
                    |builder, a, b| {
                        let func = builder.module().get_function("__nativejs_add").unwrap();

                        let re = builder.call(func, &[a.into(), b.into()]).unwrap();
                        re.into_float_value()
                    },
                );
            }
            IR::Sub(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                self.compile_arithmetic_operation(
                    builder,
                    a,
                    b,
                    |builder, a, b| builder.builder().build_float_sub(a, b, "fsub"),
                    |builder, a, b| {
                        let func = builder.module().get_function("__nativejs_sub").unwrap();

                        let re = builder.call(func, &[a.into(), b.into()]).unwrap();
                        re.into_float_value()
                    },
                );
            }
            IR::Mul(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                self.compile_arithmetic_operation(
                    builder,
                    a,
                    b,
                    |builder, a, b| builder.builder().build_float_mul(a, b, "fmul"),
                    |builder, a, b| {
                        let func = builder.module().get_function("__nativejs_mul").unwrap();

                        let re = builder.call(func, &[a.into(), b.into()]).unwrap();
                        re.into_float_value()
                    },
                );
            }
            IR::Div(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                self.compile_arithmetic_operation(
                    builder,
                    a,
                    b,
                    |builder, a, b| builder.builder().build_float_div(a, b, "fdiv"),
                    |builder, a, b| {
                        let func = builder.module().get_function("__nativejs_div").unwrap();

                        let re = builder.call(func, &[a.into(), b.into()]).unwrap();
                        re.into_float_value()
                    },
                );
            }
            IR::Mod(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                self.compile_arithmetic_operation(
                    builder,
                    a,
                    b,
                    |builder, a, b| builder.builder().build_float_rem(a, b, "frem"),
                    |builder, a, b| {
                        let func = builder.module().get_function("__nativejs_rem").unwrap();

                        let re = builder.call(func, &[a.into(), b.into()]).unwrap();
                        re.into_float_value()
                    },
                );
            }
            IR::Exp(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                let func = builder.module().get_function("__nativejs_pow").unwrap();

                let re = builder.call(func, &[a.into(), b.into()]).unwrap();
                self.write_acc(builder, re.into_float_value());
            }
            IR::RShift(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                self.compile_arithmetic_operation(
                    builder,
                    a,
                    b,
                    |builder, a, b| {
                        let a = builder.builder().build_float_to_signed_int(
                            a,
                            builder.context().i32_type(),
                            "ftoi",
                        );
                        let b = builder.builder().build_float_to_signed_int(
                            b,
                            builder.context().i32_type(),
                            "ftoi",
                        );

                        let re = builder.builder().build_right_shift(a, b, true, "rshift");

                        builder.builder().build_signed_int_to_float(
                            re,
                            builder.context().f64_type(),
                            "int_to_f64",
                        )
                    },
                    |builder, a, b| {
                        let func = builder.module().get_function("__nativejs_rshift").unwrap();

                        let re = builder.call(func, &[a.into(), b.into()]).unwrap();
                        re.into_float_value()
                    },
                );
            }
            IR::LShift(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                self.compile_arithmetic_operation(
                    builder,
                    a,
                    b,
                    |builder, a, b| {
                        let a = builder.builder().build_float_to_signed_int(
                            a,
                            builder.context().i32_type(),
                            "ftoi",
                        );
                        let b = builder.builder().build_float_to_signed_int(
                            b,
                            builder.context().i32_type(),
                            "ftoi",
                        );

                        let re = builder.builder().build_left_shift(a, b, "lshift");

                        builder.builder().build_signed_int_to_float(
                            re,
                            builder.context().f64_type(),
                            "int_to_f64",
                        )
                    },
                    |builder, a, b| {
                        let func = builder.module().get_function("__nativejs_lshift").unwrap();

                        let re = builder.call(func, &[a.into(), b.into()]).unwrap();
                        re.into_float_value()
                    },
                );
            }

            IR::ZeroFillRShift(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                self.compile_arithmetic_operation(
                    builder,
                    a,
                    b,
                    |builder, a, b| {
                        let a = builder.builder().build_float_to_signed_int(
                            a,
                            builder.context().i32_type(),
                            "ftoi",
                        );
                        let b = builder.builder().build_float_to_signed_int(
                            b,
                            builder.context().i32_type(),
                            "ftoi",
                        );

                        let re = builder.builder().build_right_shift(a, b, false, "rshift");

                        builder.builder().build_signed_int_to_float(
                            re,
                            builder.context().f64_type(),
                            "int_to_f64",
                        )
                    },
                    |builder, a, b| {
                        let func = builder
                            .module()
                            .get_function("__nativejs_unsigned_rshift")
                            .unwrap();

                        let re = builder.call(func, &[a.into(), b.into()]).unwrap();
                        re.into_float_value()
                    },
                );
            }
            IR::And(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                let a_is_true = self.is_true(builder, a);
                let b_is_true = self.is_true(builder, b);

                let is_true = builder.builder().build_and(a_is_true, b_is_true, "is_true");

                let t = builder
                    .context()
                    .f64_type()
                    .const_float(f64::from_bits(Self::FALSE));
                let f = builder
                    .context()
                    .f64_type()
                    .const_float(f64::from_bits(Self::TRUE));

                let value = builder.builder().build_select(is_true, t, f, "select");
                self.write_acc(builder, value.into_float_value());
            }
            IR::Or(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                let a_is_true = self.is_true(builder, a);

                let value = builder.builder().build_select(a_is_true, a, b, "select");
                self.write_acc(builder, value.into_float_value());
            }
            IR::BitAnd(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                let a = builder.builder().build_float_to_signed_int(
                    a,
                    builder.context().i32_type(),
                    "ftoi",
                );
                let b = builder.builder().build_float_to_signed_int(
                    b,
                    builder.context().i32_type(),
                    "ftoi",
                );

                let value = builder.builder().build_and(a, b, "band");

                let re = builder.builder().build_signed_int_to_float(
                    value,
                    builder.context().f64_type(),
                    "itof",
                );
                self.write_acc(builder, re);
            }
            IR::BitOr(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                let a = builder.builder().build_float_to_signed_int(
                    a,
                    builder.context().i32_type(),
                    "ftoi",
                );
                let b = builder.builder().build_float_to_signed_int(
                    b,
                    builder.context().i32_type(),
                    "ftoi",
                );

                let value = builder.builder().build_or(a, b, "bor");

                let re = builder.builder().build_signed_int_to_float(
                    value,
                    builder.context().f64_type(),
                    "itof",
                );
                self.write_acc(builder, re);
            }
            IR::BitXor(id) => {
                let a = self.read_temp(builder, id);
                let b = self.read_acc(builder);

                let a = builder.builder().build_float_to_signed_int(
                    a,
                    builder.context().i32_type(),
                    "ftoi",
                );
                let b = builder.builder().build_float_to_signed_int(
                    b,
                    builder.context().i32_type(),
                    "ftoi",
                );

                let value = builder.builder().build_xor(a, b, "bxor");

                let re = builder.builder().build_signed_int_to_float(
                    value,
                    builder.context().f64_type(),
                    "itof",
                );
                self.write_acc(builder, re);
            }
        }
    }
}
