use crate::types::Any;

#[no_mangle]
extern "C" fn __native_ts_initialise() {}

#[no_mangle]
extern "C" fn __native_ts_exit() {}

#[no_mangle]
extern "C" fn __native_ts_get_global_object() {}

#[no_mangle]
extern "C" fn __native_ts_print(value: Any) {}
