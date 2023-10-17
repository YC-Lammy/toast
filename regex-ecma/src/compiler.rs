use std::collections::HashMap;
use std::env::VarError;

use cranelift::codegen::ir::StackSlot;
use cranelift::codegen::settings;
use cranelift::prelude::*;

use cranelift_jit::{
    JITBuilder,
    JITModule
};
use cranelift_module::DataDescription;
use cranelift_module::Module;

use crate::ast::*;

pub struct Compiler {
    ignore_case: bool,
    multiline: bool,
    dot_all: bool,
    unicode: bool,
    capture_group_count: usize,
    group_names: HashMap<String, usize>,
    
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data description, which is to data objects what `ctx` is to functions.
    data_description: DataDescription,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,

}

impl Compiler {
    pub fn new(i: bool, m: bool, s: bool, u: bool) -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names()).expect("initialise JIT builder");

        let module = JITModule::new(builder);

        Self {
            ignore_case: i,
            multiline: m,
            dot_all: s,
            unicode: u,
            capture_group_count: 0,
            group_names: Default::default(),
        
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module: module,

        }
    }

    pub fn compile(&mut self, pat: &Pattern) {

        // input pointer
        self.ctx.func.signature.params.push(AbiParam::new(types::I64));
        // input length
        self.ctx.func.signature.params.push(AbiParam::new(types::I64));
        // index
        self.ctx.func.signature.params.push(AbiParam::new(types::I64));
        // capture group buffer
        self.ctx.func.signature.params.push(AbiParam::new(types::I64));

        // the end index, -1 if no match
        self.ctx.func.signature.returns.push(AbiParam::new(types::I64));
        
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry = builder.create_block();
        builder.append_block_params_for_function_params(entry);

        let exit = builder.create_block();
        builder.append_block_params_for_function_returns(exit);

        let input_ptr = builder.create_sized_stack_slot(
            StackSlotData { 
                kind: StackSlotKind::ExplicitSlot, 
                size: 8 * 8
            }
        );
        let input_length = builder.create_sized_stack_slot(
            StackSlotData { 
                kind: StackSlotKind::ExplicitSlot, 
                size: 8 * 8
            }
        );
        let index = builder.create_sized_stack_slot(
            StackSlotData { 
                kind: StackSlotKind::ExplicitSlot, 
                size: 8 * 8
            }
        );
        let capture_buffer = builder.create_sized_stack_slot(
            StackSlotData { 
                kind: StackSlotKind::ExplicitSlot, 
                size: 8 * 8
            }
        );
        let cursor = builder.create_sized_stack_slot(
            StackSlotData { 
                kind: StackSlotKind::ExplicitSlot, 
                size: 8 * 8
            }
        );

        builder.switch_to_block(entry);

        let params:[Value;4] = builder.block_params(entry).try_into().unwrap();

        builder.ins().stack_store(params[0], input_ptr, 0);
        builder.ins().stack_store(params[1], input_length, 0);
        builder.ins().stack_store(params[2], index, 0);
        builder.ins().stack_store(params[2], cursor, 0);
        builder.ins().stack_store(params[3], capture_buffer, 0);

        let mut t = FunctionTranslater{
            ignore_case: self.ignore_case,
            multiline: self.multiline,
            dot_all: self.dot_all,
            unicode: self.unicode,

            builder: builder,
            module: &mut self.module,
            exit,
            input_ptr,
            input_length,
            index,
            capture_buffer,
            cursor,

            variable: 0,
        };

        let succeed = t.translate_disjunction(&pat.disjunction, true);

        let mut builder = t.builder;

        {
            let current_index = builder.ins().stack_load(types::I64, index, 0);

            let failed = builder.ins().iconst(types::I64, -1);
            let end_index = builder.ins().select(succeed, current_index, failed);

            builder.ins().jump(exit, &[end_index]);
        }
        

        builder.switch_to_block(exit);

        {
            let end_index = builder.block_params(exit)[0];
            builder.ins().return_(&[end_index]);
        }

        builder.seal_block(exit);
        builder.seal_all_blocks();
    }
}

struct FunctionTranslater<'a>{
    ignore_case: bool,
    multiline: bool,
    dot_all: bool,
    unicode: bool,

    builder: FunctionBuilder<'a>,
    module: &'a mut JITModule,

    exit: Block,

    input_ptr: StackSlot,
    input_length: StackSlot,
    index: StackSlot,
    cursor: StackSlot,
    capture_buffer: StackSlot,

    variable: usize,
}

impl<'a> FunctionTranslater<'a>{
    fn translate_disjunction(&mut self, disjunction: &Disjunction, forward: bool) -> Value{
        // return true if empty
        if disjunction.alternatives.is_empty(){
            return self.builder.ins().iconst(types::I8, 1);
        }

        // the exit block
        let exit = self.builder.create_block();
        self.builder.append_block_param(exit, types::I8);

        // stores the cusor
        let cursor_store = Variable::new(self.variable);
        self.variable += 1;

        // declare variable
        self.builder.declare_var(cursor_store, types::I64);

        let mut i = 0;

        // translate all alternatives
        for alt in &disjunction.alternatives{

            // load the cursor to variable
            let c = self.cursor();
            self.builder.def_var(cursor_store, c);

            // translate the match
            let succeed = self.translate_alternative(alt, forward);

            i += 1;
            // last block
            if i == disjunction.alternatives.len(){
                // jump to exit and seal block
                self.builder.ins().jump(exit, &[succeed]);
                self.builder.seal_block(self.builder.current_block().unwrap());

            } else{

                // create new block for next iteration
                let new_block = self.builder.create_block();

                // load the old and new cursor
                let old_cursor = self.builder.use_var(cursor_store);
                let current_cursor = self.builder.ins().stack_load(types::I64, self.cursor, 0);

                // retain current cursor if succeed
                let new_cursor = self.builder.ins().select(succeed, current_cursor, old_cursor);
                self.builder.ins().stack_store(new_cursor, self.cursor, 0);

                // exit if failed
                self.builder.ins().brif(
                    succeed, 
                    exit, 
                    &[succeed], 
                    new_block, 
                    &[]
                );
                self.builder.seal_block(self.builder.current_block().unwrap());

                // switch to block
                self.builder.switch_to_block(new_block);
            }
        };

        // exit block
        self.builder.switch_to_block(exit);
        let succeed = self.builder.block_params(exit)[0];

        return succeed;
    }

    fn translate_alternative(&mut self, alternative: &Alternative, forward: bool) -> Value{
        if alternative.terms.is_empty(){
            return self.builder.ins().iconst(types::I8, 1);
        }

        let exit = self.builder.create_block();
        self.builder.append_block_param(exit, types::I8);

        let mut i = 0;

        for term in &alternative.terms{
            let succeed = match term{
                Term::Assertion(a) => {
                    self.translate_assertion(a)
                }
                Term::Atom { atom, quantifier } => {
                    if let Some(quantifier) = quantifier{
                        self.translate_atom_quantifier(atom, quantifier)
                    } else{
                        self.translate_atom(atom)
                    }
                }
            };

            i += 1;

            // last term
            if i == alternative.terms.len(){
                self.builder.ins().jump(exit, &[succeed]);
                self.builder.seal_block(self.builder.current_block().unwrap());

            } else{
                // create new block for next iteration
                let next_block = self.builder.create_block();

                // goto next block if succeed
                self.builder.ins().brif(
                    succeed, 
                    next_block, 
                    &[], 
                    exit, 
                    &[succeed]
                );
                self.builder.seal_block(self.builder.current_block().unwrap());

                // switch to next block
                self.builder.switch_to_block(next_block);

            }
        };

        self.builder.switch_to_block(exit);
        let succeed = self.builder.block_params(exit)[0];

        return succeed
    }

    fn cursor(&mut self) -> Value{
        self.builder.ins().stack_load(types::I64, self.cursor, 0)
    }

    /// returns an i8 value
    fn read_utf8_at(&mut self, index: Value) -> Value{
        let ptr = self.builder.ins().stack_load(types::I64, self.input_ptr, 0);
        let ptr = self.builder.ins().iadd(ptr, index);

        return self.builder.ins().load(types::I8, MemFlags::new(), ptr, 0);
    }

    /// returns i32 codepoint
    fn utf8_first_byte(&mut self, v: Value, width: u32) -> Value{
        let mask = 0x7Fu8 >> width;
        let v = self.builder.ins().band_imm(v, mask as i64);

        return self.builder.ins().uextend(types::I32, v)
    }

    fn utf8_acc_cont_byte(&mut self, ch:Value, byte: Value) -> Value{
        const CONT_MASK: u8 = 0x3F;

        let ch = self.builder.ins().ishl_imm(ch, 6);

        let byte = self.builder.ins().band_imm(byte, CONT_MASK as i64);

        let v = self.builder.ins().uextend(types::I32, byte);

        return self.builder.ins().bor(ch, v)
    }

    fn utf8_is_cont_byte(&mut self, byte: Value) -> Value{
        return self.builder.ins().icmp_imm(IntCC::SignedLessThan, byte, -64)
    }

    /// return the code point and the number of bytes
    fn lookback_char(&mut self) -> (Value, Value){
        let cursor = self.cursor();
        let cursor = self.builder.ins().iadd_imm(cursor, -1);

        let exit = self.builder.create_block();
        self.builder.append_block_param(exit, types::I32);
        self.builder.append_block_param(exit, types::I64);

        let two_byte_block = self.builder.create_block();
        self.builder.append_block_param(two_byte_block, types::I8);

        let three_byte_block = self.builder.create_block();
        self.builder.append_block_param(three_byte_block, types::I8);
        self.builder.append_block_param(three_byte_block, types::I8);

        let four_byte_block = self.builder.create_block();
        self.builder.append_block_param(three_byte_block, types::I8);
        self.builder.append_block_param(three_byte_block, types::I8);
        self.builder.append_block_param(three_byte_block, types::I8);

        {
            // read tail u8
            let last_byte = self.read_utf8_at(cursor);

            let is_single_code_point = self.builder.ins().icmp_imm(IntCC::UnsignedLessThan, last_byte, 128);

            let single_codepoint = self.builder.ins().uextend(types::I32, last_byte);
            let one = self.builder.ins().iconst(types::I64, 1);

            // exit if only one byte
            self.builder.ins().brif(
                is_single_code_point, 
                exit, 
                &[single_codepoint, one], 
                two_byte_block, 
                &[
                    last_byte
                ]
            );
            self.builder.seal_block(self.builder.current_block().unwrap());
        }

        // two bytes
        self.builder.switch_to_block(two_byte_block);

        {
            let last_byte = self.builder.block_params(two_byte_block)[0];

            let cursor = self.cursor();
            let cursor = self.builder.ins().iadd_imm(cursor, -2);

            let last_two_byte = self.read_utf8_at(cursor);

            let ch = self.utf8_first_byte(last_two_byte, 2);
            let ch = self.utf8_acc_cont_byte(ch, last_byte);

            let two = self.builder.ins().iconst(types::I64, 2);

            let is_cont_byte = self.utf8_is_cont_byte(last_two_byte);

            // exit if only two byte
            self.builder.ins().brif(
                is_cont_byte, 
                three_byte_block, 
                &[last_byte, last_two_byte], 
                exit, 
                &[
                    ch, two
                ]
            );
            self.builder.seal_block(two_byte_block);
        }

        // three byte block
        self.builder.switch_to_block(three_byte_block);

        {
            let last_byte = self.builder.block_params(three_byte_block)[0];
            let last_two_byte = self.builder.block_params(three_byte_block)[1];

            let cursor = self.cursor();
            let cursor = self.builder.ins().iadd_imm(cursor, -3);

            let last_three_byte = self.read_utf8_at(cursor);

            let ch = self.utf8_first_byte(last_three_byte, 3);
            let ch = self.utf8_acc_cont_byte(ch, last_two_byte);
            let ch = self.utf8_acc_cont_byte(ch, last_byte);

            let three = self.builder.ins().iconst(types::I64, 3);

            let is_cont_byte = self.utf8_is_cont_byte(last_three_byte);

            // exit if only two byte
            self.builder.ins().brif(
                is_cont_byte, 
                four_byte_block, 
                &[last_byte, last_two_byte, last_three_byte], 
                exit, 
                &[
                    ch, three
                ]
            );
            self.builder.seal_block(three_byte_block);
        };
        
        // four byte block
        self.builder.switch_to_block(four_byte_block);

        {
            let last_byte = self.builder.block_params(four_byte_block)[0];
            let last_two_byte = self.builder.block_params(four_byte_block)[1];
            let last_three_byte = self.builder.block_params(four_byte_block)[2];

            let cursor = self.cursor();
            let cursor = self.builder.ins().iadd_imm(cursor, -4);

            let last_four_byte = self.read_utf8_at(cursor);

            let ch = self.utf8_first_byte(last_four_byte, 4);
            let ch = self.utf8_acc_cont_byte(ch, last_three_byte);
            let ch = self.utf8_acc_cont_byte(ch, last_two_byte);
            let ch = self.utf8_acc_cont_byte(ch, last_byte);

            let four = self.builder.ins().iconst(types::I64, 4);

            self.builder.ins().jump(
                exit, 
                &[
                    ch, four
                ]
            );
            self.builder.seal_block(three_byte_block);
        };

        self.builder.switch_to_block(exit);

        let char_code = self.builder.block_params(exit)[0];
        let char_len = self.builder.block_params(exit)[1];

        return (char_code, char_len)
    }

    fn translate_assertion(&mut self, assertion:&Assertion) -> Value{
        match assertion{
            Assertion::Beginning => {
                let cursor = self.cursor();
                let is_cursor_zero = self.builder.ins().icmp_imm(IntCC::Equal, cursor, 0);

                if self.multiline{
                    let (last_char, _char_len) = self.lookback_char();

                    let is_line_break = self.builder.ins().icmp_imm(IntCC::Equal, last_char, '\n' as i64);

                    return self.builder.ins().bor(is_cursor_zero, is_line_break)
                } else{
                    return is_cursor_zero
                }
            },
            _ => todo!()
        }
    }

    fn translate_atom_quantifier(&mut self, atom: &Atom, quantifier: &Quantifier) -> Value{
        
        todo!()
    }

    fn translate_atom(&mut self, atom: &Atom) -> Value{
        todo!()
    }
}