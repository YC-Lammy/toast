use core::ffi::{c_int, c_void};
use core::ptr;
use core::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};

use gimli::{constants, NativeEndian};
use gimli::{EndianSlice, Error, Pointer, Reader};
use unwinding::abi::{UnwindAction, UnwindContext, UnwindReasonCode};

use crate::types::Any;

/// LAM\0NATS
pub const NATIVE_TS_EXCEPTION_CLASS: u64 = u64::from_ne_bytes([
    'L' as u8, 'A' as u8, 'M' as u8, '\0' as u8, 'N' as u8, 'A' as u8, 'T' as u8, 'S' as u8,
]);

#[derive(Debug)]
enum EHAction {
    None,
    Cleanup(usize),
    Catch(usize),
}

pub struct NativeTsException {
    pub header: unwinding::abi::UnwindException,
    // whether it is being thrown
    pub thrown: bool,
    // handler count
    pub handler_count: usize,
    pub next_exception: *mut NativeTsException,
    /// native-ts specific memory
    pub exception_value: Any,
}

pub type StaticSlice = EndianSlice<'static, NativeEndian>;

pub unsafe fn get_unlimited_slice<'a>(start: *const u8) -> &'a [u8] {
    // Create the largest possible slice for this address.
    let start = start as usize;
    let end = start.saturating_add(isize::MAX as _);
    let len = end - start;
    unsafe { core::slice::from_raw_parts(start as *const _, len) }
}

pub unsafe fn deref_pointer(ptr: Pointer) -> usize {
    match ptr {
        Pointer::Direct(x) => x as _,
        Pointer::Indirect(x) => unsafe { *(x as *const _) },
    }
}

fn parse_pointer_encoding(input: &mut StaticSlice) -> gimli::Result<constants::DwEhPe> {
    let eh_pe = input.read_u8()?;
    let eh_pe = constants::DwEhPe(eh_pe);

    if eh_pe.is_valid_encoding() {
        Ok(eh_pe)
    } else {
        Err(gimli::Error::UnknownPointerEncoding)
    }
}

fn parse_encoded_pointer(
    encoding: constants::DwEhPe,
    unwind_ctx: &UnwindContext<'_>,
    input: &mut StaticSlice,
) -> gimli::Result<Pointer> {
    if encoding == constants::DW_EH_PE_omit {
        return Err(Error::CannotParseOmitPointerEncoding);
    }

    let base = match encoding.application() {
        constants::DW_EH_PE_absptr => 0,
        constants::DW_EH_PE_pcrel => input.slice().as_ptr() as u64,
        constants::DW_EH_PE_textrel => unwinding::abi::_Unwind_GetTextRelBase(unwind_ctx) as u64,
        constants::DW_EH_PE_datarel => unwinding::abi::_Unwind_GetDataRelBase(unwind_ctx) as u64,
        constants::DW_EH_PE_funcrel => unwinding::abi::_Unwind_GetRegionStart(unwind_ctx) as u64,
        constants::DW_EH_PE_aligned => return Err(Error::UnsupportedPointerEncoding),
        _ => unreachable!(),
    };

    let offset = match encoding.format() {
        constants::DW_EH_PE_absptr => input.read_address(core::mem::size_of::<usize>() as _),
        constants::DW_EH_PE_uleb128 => input.read_uleb128(),
        constants::DW_EH_PE_udata2 => input.read_u16().map(u64::from),
        constants::DW_EH_PE_udata4 => input.read_u32().map(u64::from),
        constants::DW_EH_PE_udata8 => input.read_u64(),
        constants::DW_EH_PE_sleb128 => input.read_sleb128().map(|a| a as u64),
        constants::DW_EH_PE_sdata2 => input.read_i16().map(|a| a as u64),
        constants::DW_EH_PE_sdata4 => input.read_i32().map(|a| a as u64),
        constants::DW_EH_PE_sdata8 => input.read_i64().map(|a| a as u64),
        _ => unreachable!(),
    }?;

    let address = base.wrapping_add(offset);
    Ok(if encoding.is_indirect() {
        Pointer::Indirect(address)
    } else {
        Pointer::Direct(address)
    })
}

fn find_eh_action(
    reader: &mut StaticSlice,
    unwind_ctx: &UnwindContext<'_>,
) -> gimli::Result<EHAction> {
    let func_start = unwinding::abi::_Unwind_GetRegionStart(unwind_ctx);
    let mut ip_before_instr = 0;
    let ip = unwinding::abi::_Unwind_GetIPInfo(unwind_ctx, &mut ip_before_instr);
    let ip = if ip_before_instr != 0 { ip } else { ip - 1 };

    let start_encoding = parse_pointer_encoding(reader)?;
    let lpad_base = if !start_encoding.is_absent() {
        unsafe { deref_pointer(parse_encoded_pointer(start_encoding, unwind_ctx, reader)?) }
    } else {
        func_start
    };

    let ttype_encoding = parse_pointer_encoding(reader)?;
    if !ttype_encoding.is_absent() {
        reader.read_uleb128()?;
    }

    let call_site_encoding = parse_pointer_encoding(reader)?;
    let call_site_table_length = reader.read_uleb128()?;
    reader.truncate(call_site_table_length as _)?;

    while !reader.is_empty() {
        let cs_start = unsafe {
            deref_pointer(parse_encoded_pointer(
                call_site_encoding,
                unwind_ctx,
                reader,
            )?)
        };
        let cs_len = unsafe {
            deref_pointer(parse_encoded_pointer(
                call_site_encoding,
                unwind_ctx,
                reader,
            )?)
        };
        let cs_lpad = unsafe {
            deref_pointer(parse_encoded_pointer(
                call_site_encoding,
                unwind_ctx,
                reader,
            )?)
        };
        let cs_action = reader.read_uleb128()?;
        if ip < func_start + cs_start {
            break;
        }
        if ip < func_start + cs_start + cs_len {
            if cs_lpad == 0 {
                return Ok(EHAction::None);
            } else {
                let lpad = lpad_base + cs_lpad;
                return Ok(match cs_action {
                    0 => EHAction::Cleanup(lpad),
                    _ => EHAction::Catch(lpad),
                });
            }
        }
    }
    Ok(EHAction::None)
}

#[no_mangle]
pub extern "C" fn __native_ts_eh_personality(
    version: c_int,
    actions: UnwindAction,
    exception_class: u64,
    exception: &mut NativeTsException,
    context: &mut UnwindContext,
) -> UnwindReasonCode {
    // version of the unwind library must be 1
    if version != 1 {
        return UnwindReasonCode::FATAL_PHASE1_ERROR;
    }

    // check the exception class
    if exception_class != NATIVE_TS_EXCEPTION_CLASS {
        // ignore foreign exception
        return UnwindReasonCode::CONTINUE_UNWIND;
    }

    let lsda = unwinding::abi::_Unwind_GetLanguageSpecificData(&context);
    if lsda.is_null() {
        return UnwindReasonCode::CONTINUE_UNWIND;
    }

    let mut lsda = EndianSlice::new(
        unsafe { get_unlimited_slice(lsda as *const u8) },
        NativeEndian,
    );
    let eh_action = match find_eh_action(&mut lsda, &context) {
        Ok(v) => v,
        Err(_) => return UnwindReasonCode::FATAL_PHASE1_ERROR,
    };

    // in the search phase, phase 1
    if actions.contains(UnwindAction::SEARCH_PHASE) {
        match eh_action {
            // no catch is found, continue unwind
            EHAction::None | EHAction::Cleanup(_) => return UnwindReasonCode::CONTINUE_UNWIND,
            // a handler is found
            EHAction::Catch(_) => return UnwindReasonCode::HANDLER_FOUND,
        }
    }

    // should be in cleanup phase, phase 2
    if !actions.contains(UnwindAction::CLEANUP_PHASE) {
        return UnwindReasonCode::FATAL_PHASE2_ERROR;
    }

    // the catch clause is not allowed to catch the exception
    // only procceed to cleanup and resume exception
    if actions.contains(UnwindAction::FORCE_UNWIND) {}
    // the handler frame
    if actions.contains(UnwindAction::HANDLER_FRAME) {}

    if actions.contains(UnwindAction::END_OF_STACK) {}

    match eh_action {
        // no action is required
        EHAction::None => return UnwindReasonCode::CONTINUE_UNWIND,
        // setup the context and transfer to landingpad
        EHAction::Catch(landingpad) | EHAction::Cleanup(landingpad) => {
            // set the ip to the landing pad
            unwinding::abi::_Unwind_SetIP(context, landingpad);

            #[cfg(target_arch = "x86_64")]
            let regs = (gimli::X86_64::RAX, gimli::X86_64::RDX);
            #[cfg(target_arch = "x86")]
            let regs = (gimli::X86::EAX, gimli::X86::EDX);
            #[cfg(any(target_arch = "riscv64", target_arch = "riscv32"))]
            let regs = (gimli::RiscV::A0, gimli::RiscV::A1);
            #[cfg(target_arch = "aarch64")]
            let regs = (gimli::AArch64::X0, gimli::AArch64::X1);
            #[cfg(not(any(
                target_arch = "x86_64",
                target_arch = "x86",
                target_arch = "riscv64",
                target_arch = "riscv32",
                target_arch = "aarch64"
            )))]
            compile_error!("unsupported target");

            // forward the exception
            unwinding::abi::_Unwind_SetGR(context, regs.0 .0 as _, exception as *mut _ as usize);
            unwinding::abi::_Unwind_SetGR(context, regs.1 .0 as _, 0);

            return UnwindReasonCode::INSTALL_CONTEXT;
        }
    }
}

#[no_mangle]
pub extern "C" fn __native_ts_allocate_exception(value: Any) -> *mut NativeTsException {
    unsafe {
        let exception =
            libc::malloc(core::mem::size_of::<NativeTsException>()) as *mut NativeTsException;
        ptr::write(
            exception,
            NativeTsException {
                header: core::mem::zeroed(),
                thrown: false,
                handler_count: 0,
                next_exception: 0 as _,
                exception_value: value,
            },
        );
        return exception;
    }
}
#[no_mangle]
pub extern "C" fn __native_ts_free_exception(exception: &mut NativeTsException) {
    unsafe {
        libc::free(exception as *mut _ as *mut c_void);
    }
}

static UNCAUGHT_EXCEPTION: AtomicUsize = AtomicUsize::new(0);
static STACK_TOP: AtomicPtr<NativeTsException> = AtomicPtr::new(0 as _);

#[no_mangle]
pub extern "C" fn __native_ts_throw(exception: &mut NativeTsException) {
    UNCAUGHT_EXCEPTION.fetch_add(1, Ordering::SeqCst);
    exception.header.exception_class = NATIVE_TS_EXCEPTION_CLASS;
    exception.thrown = true;

    unsafe {
        unwinding::abi::_Unwind_RaiseException(&mut exception.header);
    }
}

#[no_mangle]
pub extern "C" fn __native_ts_begin_catch(exception: &mut NativeTsException) {
    exception.handler_count += 1;
    UNCAUGHT_EXCEPTION.fetch_sub(1, Ordering::SeqCst);

    let current_top = STACK_TOP.load(Ordering::SeqCst);
    exception.next_exception = current_top;

    STACK_TOP.store(exception, Ordering::SeqCst);
}

#[no_mangle]
pub extern "C" fn __native_ts_end_catch() {
    let top = STACK_TOP.load(Ordering::SeqCst);

    if let Some(exception) = unsafe { top.as_mut() } {
        // decrement handler count
        let _ = exception.handler_count.checked_sub(1);

        if exception.handler_count == 0 {
            // pop from stack
            STACK_TOP.store(exception.next_exception, Ordering::SeqCst);
        }
        if exception.handler_count == 0 && !exception.thrown {
            // destroy exception
        }
    }
}

#[no_mangle]
pub extern "C" fn __native_ts_rethrow() {
    let top = STACK_TOP.load(Ordering::SeqCst);

    if let Some(exception) = unsafe { top.as_mut() } {
        exception.thrown = true;
    }
}

#[no_mangle]
pub extern "C" fn __native_ts_unwind_resume(exception: &mut NativeTsException) {
    unsafe { unwinding::abi::_Unwind_Resume(&mut exception.header) }
}
