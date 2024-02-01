
declare function TS_enter_try_catch();
declare function TS_exit_try_catch();
declare function TS_throw(value:any);
declare function TS_check_error():boolean;
declare function TS_get_error():any;

declare function TS_gc_malloc(size:null): null;

declare function TS_object_get(obj:{}, key:string);
declare function TS_object_set(obj:{}, key:string, value:any);
declare function TS_object_get_fn_ptr(obj:{}):null;