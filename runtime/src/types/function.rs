
use super::Any;

use alloc::vec::Vec;
use iron_gc::GcPtr;
use iron_gc::Array;

#[repr(C)]
pub struct FunctionDescriptor{
    pub num_args: u8,
    pub var_arg: bool,
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct TSFuncPtr(
    pub unsafe extern "C" fn(
        this: Any,
        captures: *const *const Any,
        argc: u32,
        arg1: Any,
        arg2: Any,
        arg3: Any,
        args: ...
    ) -> Any,
);

#[derive(Clone)]
#[repr(C)]
pub struct Closure {
    pub ptr: TSFuncPtr,
    pub this: Option<Any>,
    pub captures: GcPtr<Array<GcPtr<Any>>>,
}

#[derive(Clone)]
#[repr(C)]
pub struct FunctionBind {
    ptr: TSFuncPtr,
    this: Any,
    captures: GcPtr<Array<Any>>,
    args: Vec<Any>,
    arg_len: usize
}

unsafe impl Sync for TSFuncPtr {}
unsafe impl Send for TSFuncPtr {}
unsafe impl Sync for Closure {}
unsafe impl Send for Closure {}
unsafe impl Sync for FunctionBind{}
unsafe impl Send for FunctionBind{}

impl TSFuncPtr {
    #[inline]
    pub fn get_inner(
        self,
    ) -> unsafe extern "C" fn(
        this: Any,
        captures: *const *const Any,
        argc: u32,
        arg1: Any,
        arg2: Any,
        arg3: Any,
        args: ...
    ) -> Any {
        return self.0;
    }

    #[inline]
    pub fn dynamic_call(
        self,
        this: Any,
        captures: *const *const Any,
        argc: u32,
        args: *const Any,
    ) -> Any {
        unsafe {
            struct LazyIndex<'a>{
                s:&'a [Any],
            }
            impl<'a> core::ops::Index<usize> for LazyIndex<'a>{
                type Output = Any;
                #[inline]
                fn index(&self, index: usize) -> &Self::Output {
                    return unsafe{self.s.get_unchecked(index)}
                }
            }

            let args = core::slice::from_raw_parts(args, argc as usize);
            let args = LazyIndex{s:args};
            let u = Any::UNDEFINED;

            match argc {
                0 => self.0(this, captures, argc, u, u, u),
                1 => self.0(this, captures, argc, args[0], u, u),
                2 => self.0(this, captures, argc, args[0], args[1], u),
                3 => self.0(this, captures, argc, args[0], args[1], args[2]),
                4 => self.0(this, captures, argc, args[0], args[1], args[2], args[3]),
                5 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4],
                ),
                6 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                ),
                7 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6],
                ),
                8 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7],
                ),
                9 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8],
                ),
                10 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9],
                ),
                11 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10],
                ),
                12 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11],
                ),
                13 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12]
                ),
                14 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13]
                ),
                15 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14]
                ),
                16 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15]
                ),
                17 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16]
                ),
                18 => self.0(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17]
                ),
                _ => todo!(),
            }
        }
    }
}

impl Closure{
    #[inline]
    pub fn call(&self, this:Any, args:&[Any]) -> Any{
        return self.ptr.dynamic_call(self.this.unwrap_or(this), self.captures.as_ptr(), args.len() as u32, args.as_ptr())
    }
}

impl FunctionBind{
    #[inline]
    pub fn call(&self, args:&[Any]) -> Any{
        let argc = self.arg_len + args.len();

        // call directly withe the saved args
        if args.len() == 0{
            self.ptr.dynamic_call(self.this, self.captures.as_double_ptr(), argc as u32, self.args.as_ptr())
        } else{
            // slowpath: call with a newly allocated array
            unsafe{
                let ptr = GcPtr::<Any>::malloc_array(argc); 
                // copy the saved args
                core::ptr::copy_nonoverlapping(self.args.as_ptr(), ptr.as_mut(), self.arg_len);
                // copy the new args
                core::ptr::copy_nonoverlapping(args.as_ptr(), ptr.as_mut().add(self.arg_len), args.len());

                self.ptr.dynamic_call(self.this, self.captures.as_double_ptr(), argc as u32, ptr.as_ptr())
            } 
        }
    }
}

#[test]
pub fn test_dynamic_call() {
    unsafe extern "C" fn hello(
        this: Any,
        captures: *const *const Any,
        argc: u32,
        arg1: Any,
        arg2: Any,
        arg3: Any,
        _args: ...
    ) -> Any {
        assert!(captures as usize == 0);
        assert!(argc == 1);
        assert!(this.is_undefined());
        assert!(arg1.is_true());
        assert!(arg2.is_undefined());
        assert!(arg3.is_undefined());
        return Any::UNDEFINED;
    }

    let ptr = TSFuncPtr(hello);
    ptr.dynamic_call(Any::UNDEFINED, 0 as *const _, 1, &Any::TRUE);
}