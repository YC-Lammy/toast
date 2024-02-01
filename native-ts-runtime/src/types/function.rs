use core::ptr::NonNull;
use core::sync::atomic::AtomicU32;
use core::sync::atomic::Ordering;

use super::Any;

use alloc::boxed::Box;
use iron_gc::Trace;
use iron_gc::Visitor;

type FuncPtr = extern "C" fn(
        this: Any,
        captures: *const HeapVariable,
        argc: u32,
        arg1: Any,
        arg2: Any,
        arg3: Any,
        args: ...
    ) -> Any;

/// *const Any
#[repr(C)]
pub struct HeapVariable(NonNull<HeapVariableInner>);

struct HeapVariableInner{
    value: Any,
    rc: AtomicU32,
}

unsafe impl Sync for HeapVariable{}
unsafe impl Send for HeapVariable{}

impl Clone for HeapVariable{
    fn clone(&self) -> Self {
        unsafe{
            let inner = self.0.as_ref();
            inner.rc.fetch_add(1, Ordering::AcqRel);
        };

        return Self(self.0)
    }
}

impl Drop for HeapVariable{
    fn drop(&mut self) {
        unsafe{
            let inner = self.0.as_ref();
            let count = inner.rc.fetch_sub(1, Ordering::AcqRel);
            if count <= 1{
                libc::free(self.0.as_ptr() as _);
            }
        }
    }
}

impl Trace for HeapVariable{
    fn trace(&mut self, visitor: &mut Visitor) {
        unsafe{
            let inner = self.0.as_ref();
            let mut value = inner.value;
            value.trace(visitor);
        }
    }
}

impl HeapVariable{
    pub fn new(value: Any) -> HeapVariable{
        unsafe{
            let ptr = libc::malloc(core::mem::size_of::<HeapVariableInner>()) as *mut HeapVariableInner;
            ptr.write(
                HeapVariableInner { value, rc: AtomicU32::new(1) }
            );

            return HeapVariable(NonNull::new_unchecked(ptr))
        }
    }

    pub fn value(&self) -> Any{
        unsafe{
            let inner = self.0.as_ref();
            return inner.value
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct JSFunc{
    num_args: u16,
    var_args: bool,
    func_ptr: NonNull<()>,
}

#[derive(Clone)]
#[repr(C)]
pub struct Closure {
    pub func: JSFunc,
    pub this: Option<Any>,
    pub captures: Box<[HeapVariable]>,
}

impl Trace for Closure{
    fn trace(&mut self, visitor: &mut Visitor) {
        self.this.trace(visitor);
        for v in self.captures.iter(){
            v.value().trace(visitor);
        }
    }
}

impl Closure{
    #[inline]
    pub fn call(&self, this:Any, args:&[Any]) -> Any{
        return self.func.dynamic_call(self.this.unwrap_or(this), self.captures.as_ptr(), args.len() as u32, args.as_ptr())
    }
}

#[derive(Clone)]
#[repr(C)]
pub struct FunctionBind {
    ptr: JSFunc,
    this: Any,
    captures: Box<[HeapVariable]>,
    args: Box<[Any]>,
}

impl iron_gc::Trace for FunctionBind{
    fn trace(&mut self, visitor: &mut Visitor){
        self.this.trace(visitor);

        for v in self.captures.iter(){
            v.value().trace(visitor);
        }

        for arg in self.args.iter(){
            let mut arg = *arg;
            arg.trace(visitor);
        }
    }
}

impl FunctionBind{
    #[inline]
    pub fn call(&self, args:&[Any]) -> Any{
        let argc = self.args.len() + args.len();

        // call directly withe the saved args
        if args.len() == 0{
            self.ptr.dynamic_call(self.this, self.captures.as_ptr(), argc as u32, self.args.as_ptr())
        } else{
            // slowpath: call with a newly allocated array
            unsafe{
                let ptr = libc::malloc(argc * 8) as *mut Any;
                // copy the saved args
                core::ptr::copy_nonoverlapping(self.args.as_ptr(), ptr, self.args.len());
                // copy the new args
                core::ptr::copy_nonoverlapping(args.as_ptr(), ptr.add(self.args.len()), args.len());

                let re = self.ptr.dynamic_call(self.this, self.captures.as_ptr(), argc as u32, ptr);

                libc::free(ptr as _);

                return re;
            } 
        }
    }
}

unsafe impl Sync for JSFunc {}
unsafe impl Send for JSFunc {}
unsafe impl Sync for Closure {}
unsafe impl Send for Closure {}
unsafe impl Sync for FunctionBind{}
unsafe impl Send for FunctionBind{}

impl JSFunc {
    #[inline]
    pub fn get_inner(&self) -> FuncPtr{
        return unsafe{core::mem::transmute(self.func_ptr)};
    }

    #[inline]
    pub fn dynamic_call(
        self,
        this: Any,
        captures: *const HeapVariable,
        argc: u32,
        args: *const Any,
    ) -> Any {
        unsafe {
            #[repr(transparent)]
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

            let func = self.get_inner();

            let args = core::slice::from_raw_parts(args, argc as usize);
            let args = LazyIndex{s:args};
            let u = Any::UNDEFINED;

            if self.num_args < 4{
                if argc > self.num_args as u32 && !self.var_args{
                    return func(this, captures, self.num_args as u32, args[0], args[1], args[2])
                }
            }

            match argc {
                0 => func(this, captures, argc, u, u, u),
                1 => func(this, captures, argc, args[0], u, u),
                2 => func(this, captures, argc, args[0], args[1], u),
                3 => func(this, captures, argc, args[0], args[1], args[2]),
                4 => func(this, captures, argc, args[0], args[1], args[2], args[3]),
                5 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4],
                ),
                6 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                ),
                7 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6],
                ),
                8 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7],
                ),
                9 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8],
                ),
                10 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9],
                ),
                11 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10],
                ),
                12 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11],
                ),
                13 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12]
                ),
                14 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13]
                ),
                15 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14]
                ),
                16 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15]
                ),
                17 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16]
                ),
                18 => func(
                    this, captures, argc, args[0], args[1], args[2], args[3], args[4], args[5],
                    args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15], args[16], args[17]
                ),
                _ => todo!(),
            }
        }
    }
}

#[test]
pub fn test_dynamic_call() {
    unsafe extern "C" fn hello(
        this: Any,
        captures: *const HeapVariable,
        argc: u32,
        arg1: Any,
        arg2: Any,
        arg3: Any,
        _args: ...
    ) -> Any {
        assert!(captures as usize == 0);
        assert!(argc == 1);
        assert!(this.is_undefined());
        assert!(arg1.is_bool());
        assert!(arg2.is_undefined());
        assert!(arg3.is_undefined());
        return Any::UNDEFINED;
    }

    let ptr = JSFunc{
        num_args: 3,
        var_args: false,
        func_ptr: NonNull::new(hello as _).unwrap()
    };

    ptr.dynamic_call(Any::UNDEFINED, 0 as *const _, 1, &Any::TRUE);
}