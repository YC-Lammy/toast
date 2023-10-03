use crate::types::Any;

#[no_mangle]
extern "C" fn __nativejs_initialise(){
    iron_gc::initialise();
}

#[no_mangle]
extern "C" fn __nativejs_get_global_object(){

}

#[no_mangle]
extern "C" fn __nativejs_exit(){
    
}

#[no_mangle]
extern "C" fn __nativejs_print(value: Any){
}

#[no_mangle]
extern "C" fn __nativejs_raise_exception(exception: Any){

}