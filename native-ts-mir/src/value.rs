use std::marker::PhantomData;

use paste::paste;

pub use crate::types::*;
use crate::util::ValueID;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Value<'ctx, 'func, T: MarkerType<'ctx>> {
    pub(crate) id: ValueID,
    pub(crate) ty: T,
    pub(crate) _mark: PhantomData<(&'func (), &'ctx ())>,
}

impl<'ctx, 'func, T: MarkerType<'ctx>> Value<'ctx, 'func, T> {
    pub fn ty(&self) -> &T {
        &self.ty
    }
    pub fn id(&self) -> ValueID {
        self.id
    }
}

macro_rules! impl_into_auto {
    ($ty:ty) => {
        impl<'ctx, 'func> Into<Value<'ctx, 'func, Auto<'ctx>>> for Value<'ctx, 'func, $ty> {
            fn into(self) -> Value<'ctx, 'func, Auto<'ctx>> {
                Value {
                    id: self.id,
                    ty: Auto {
                        inner: self.ty.to_type(),
                    },
                    _mark: PhantomData,
                }
            }
        }
    };
}

impl_into_auto!(Void);
impl_into_auto!(U8);
impl_into_auto!(U16);
impl_into_auto!(U32);
impl_into_auto!(U64);
impl_into_auto!(Usize);
impl_into_auto!(I8);
impl_into_auto!(I16);
impl_into_auto!(I32);
impl_into_auto!(I64);
impl_into_auto!(Isize);
impl_into_auto!(F32);
impl_into_auto!(F64);
impl_into_auto!(Aggregate<'ctx>);
impl_into_auto!(Interface<'ctx>);

impl<'ctx, 'func, T: MarkerType<'ctx>> Into<Value<'ctx, 'func, Auto<'ctx>>>
    for Value<'ctx, 'func, Pointer<T>>
{
    fn into(self) -> Value<'ctx, 'func, Auto<'ctx>> {
        Value {
            id: self.id,
            ty: Auto {
                inner: self.ty.to_type(),
            },
            _mark: PhantomData,
        }
    }
}
impl<'ctx, 'func, Arg: FunctionArgs<'ctx>, R: MarkerType<'ctx>> Into<Value<'ctx, 'func, Auto<'ctx>>>
    for Value<'ctx, 'func, Function<'ctx, Arg, R>>
{
    fn into(self) -> Value<'ctx, 'func, Auto<'ctx>> {
        Value {
            id: self.id,
            ty: Auto {
                inner: self.ty.to_type(),
            },
            _mark: PhantomData,
        }
    }
}
impl<'ctx, 'func, T: MarkerType<'ctx>> Into<Value<'ctx, 'func, Auto<'ctx>>>
    for Value<'ctx, 'func, Array<T>>
{
    fn into(self) -> Value<'ctx, 'func, Auto<'ctx>> {
        Value {
            id: self.id,
            ty: Auto {
                inner: self.ty.to_type(),
            },
            _mark: PhantomData,
        }
    }
}
impl<'ctx, 'func, T: MarkerType<'ctx>> Into<Value<'ctx, 'func, Auto<'ctx>>>
    for Value<'ctx, 'func, Future<T>>
{
    fn into(self) -> Value<'ctx, 'func, Auto<'ctx>> {
        Value {
            id: self.id,
            ty: Auto {
                inner: self.ty.to_type(),
            },
            _mark: PhantomData,
        }
    }
}
impl<'ctx, 'func, T: ScalarMarkerType, const N: usize> Into<Value<'ctx, 'func, Auto<'ctx>>>
    for Value<'ctx, 'func, SIMD<T, N>>
where
    simd::LaneCount<N>: simd::SupportedLaneCount,
{
    fn into(self) -> Value<'ctx, 'func, Auto<'ctx>> {
        Value {
            id: self.id,
            ty: Auto {
                inner: self.ty.to_type(),
            },
            _mark: PhantomData,
        }
    }
}

macro_rules! from_auto {
    ($ty:ident) => {
        paste! {
            pub fn [<into_ $ty:lower>](&self) -> Value<'ctx, 'func, $ty>{
                if let Type::$ty = self.ty.to_type(){
                    return Value{
                        id: self.id,
                        ty: $ty,
                        _mark: PhantomData
                    }
                }
                panic!(stringify!(value is not $ty))
            }
        }
    };
}

impl<'ctx, 'func, T: MarkerType<'ctx>> Value<'ctx, 'func, T> {
    pub fn into_auto(&self) -> Value<'ctx, 'func, Auto<'ctx>> {
        Value {
            id: self.id,
            ty: Auto {
                inner: self.ty.to_type(),
            },
            _mark: PhantomData,
        }
    }
}

impl<'ctx, 'func> Value<'ctx, 'func, Function<'ctx, AutoArgs<'ctx>, Auto<'ctx>>> {
    pub fn into_function<A: FunctionArgs<'ctx>, R: MarkerType<'ctx>>(
        &self,
        args: A,
        return_: R,
    ) -> Value<'ctx, 'func, Function<'ctx, A, R>> {
        if self.ty.args.len() != args.len() {
            panic!("value is function, but wrong number of arguments provided")
        }
        for (i, ty) in self.ty.args.0.iter().enumerate() {
            if ty != &args.get(i) {
                panic!("value is function, but argument {} has wrong type", i)
            }
        }
        if self.ty.return_.inner != return_.to_type() {
            panic!("value is function, but return type does not match")
        }

        return Value {
            id: self.id,
            ty: Function {
                args,
                return_,
                _mark: PhantomData,
            },
            _mark: PhantomData,
        };
    }
}

impl<'ctx, 'func> Value<'ctx, 'func, Auto<'ctx>> {
    from_auto!(Void);
    from_auto!(U8);
    from_auto!(U16);
    from_auto!(U32);
    from_auto!(U64);
    from_auto!(Usize);
    from_auto!(I8);
    from_auto!(I16);
    from_auto!(I32);
    from_auto!(I64);
    from_auto!(Isize);
    pub fn into_aggregate(&self) -> Value<'ctx, 'func, Aggregate<'ctx>> {
        if let Type::Aggregate(id) = self.ty.inner {
            return Value {
                id: self.id,
                ty: Aggregate(id),
                _mark: PhantomData,
            };
        }
        panic!("value is not aggregate")
    }
    pub fn into_interface(&self) -> Value<'ctx, 'func, Interface<'ctx>> {
        if let Type::Interface(id) = self.ty.inner {
            return Value {
                id: self.id,
                ty: Interface(id),
                _mark: PhantomData,
            };
        }
        panic!("value is not interface")
    }
    pub fn into_pointer<T: MarkerType<'ctx>>(&self, ty: T) -> Value<'ctx, 'func, Pointer<T>> {
        if let Type::Pointer(t) = &self.ty.inner {
            if t.as_ref() == &ty.to_type() {
                return Value {
                    id: self.id,
                    ty: Pointer { pointee: ty },
                    _mark: PhantomData,
                };
            }
        }
        panic!("value is not pointer")
    }

    pub fn into_smart_pointer<T: MarkerType<'ctx>>(&self, ty: T) -> Value<'ctx, 'func, Smart<T>> {
        if let Type::Pointer(t) = &self.ty.inner {
            if t.as_ref() == &ty.to_type() {
                return Value {
                    id: self.id,
                    ty: Smart { pointee: ty },
                    _mark: PhantomData,
                };
            }
        }
        panic!("value is not pointer")
    }

    pub fn into_function<Arg: FunctionArgs<'ctx>, R: MarkerType<'ctx>>(
        &self,
        args: Arg,
        return_: R,
    ) -> Value<'ctx, 'func, Function<'ctx, Arg, R>> {
        if let Type::Function(f) = &self.ty.inner {
            if f.params.len() != args.len() {
                panic!("value is function, but wrong number of arguments provided")
            }
            for (i, ty) in f.params.iter().enumerate() {
                if ty != &args.get(i) {
                    panic!("value is function, but argument {} has wrong type", i)
                }
            }
            if f.return_ != return_.to_type() {
                panic!("value is function, but return type does not match")
            }

            return Value {
                id: self.id,
                ty: Function {
                    args,
                    return_,
                    _mark: PhantomData,
                },
                _mark: PhantomData,
            };
        }
        panic!("value is not function")
    }

    pub fn into_array<T: MarkerType<'ctx>>(&self, ty: T) -> Value<'ctx, 'func, Array<T>> {
        if let Type::Array(a) = &self.ty.inner {
            if &a.0 != &ty.to_type() {
                panic!("value is array but element type does not match")
            }
            return Value {
                id: self.id,
                ty: Array {
                    element: ty,
                    length: a.1,
                },
                _mark: PhantomData,
            };
        }
        panic!("value is not array")
    }

    pub fn into_future<T: MarkerType<'ctx>>(&self, ty: T) -> Value<'ctx, 'func, Future<T>> {
        if let Type::Future(t) = &self.ty.inner {
            if t.as_ref() == &ty.to_type() {
                return Value {
                    id: self.id,
                    ty: Future { value: ty },
                    _mark: PhantomData,
                };
            }
        }
        panic!("value is not future")
    }
    pub fn into_generator<Y: MarkerType<'ctx>, RE: MarkerType<'ctx>, R: MarkerType<'ctx>>(
        &self,
        yield_ty: Y,
        resume_ty: RE,
        return_ty: R,
    ) -> Value<'ctx, 'func, Generator<Y, RE, R>> {
        if let Type::Generator(gen) = &self.ty.inner {
            if &gen.0 != &yield_ty.to_type()
                || &gen.1 != &resume_ty.to_type()
                || &gen.2 != &return_ty.to_type()
            {
                panic!("value is generator but type does not match")
            }

            return Value {
                id: self.id,
                ty: Generator {
                    yield_: yield_ty,
                    resume: resume_ty,
                    return_: return_ty,
                },
                _mark: PhantomData,
            };
        }
        panic!("value is not generator")
    }

    pub fn into_simd<I: ScalarMarkerType, const N: usize>(&self) -> Value<'ctx, 'func, SIMD<I, N>>
    where
        simd::LaneCount<N>: simd::SupportedLaneCount,
    {
        let valid = match &self.ty.inner {
            Type::SIMDx2(t) => t == &I::TY && N == 2,
            Type::SIMDx4(t) => t == &I::TY && N == 4,
            Type::SIMDx8(t) => t == &I::TY && N == 8,
            Type::SIMDx16(t) => t == &I::TY && N == 16,
            Type::SIMDx32(t) => t == &I::TY && N == 32,
            Type::SIMDx64(t) => t == &I::TY && N == 64,
            Type::SIMDx128(t) => t == &I::TY && N == 128,
            Type::SIMDx256(t) => t == &I::TY && N == 256,
            _ => false,
        };
        if valid {
            return Value {
                id: self.id,
                ty: SIMD::default(),
                _mark: PhantomData,
            };
        }
        panic!("value is not SIMD")
    }
}
