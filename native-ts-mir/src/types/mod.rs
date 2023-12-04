pub mod aggregate;

use std::marker::PhantomData;

use crate::util::{AggregateID, InterfaceID};
use crate::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ScalarType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}

impl ScalarType {
    pub fn is_signed(&self) -> bool {
        match self {
            Self::U8 | Self::U16 | Self::U32 | Self::U64 => false,
            _ => true,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            Self::F32 | Self::F64 => true,
            _ => false,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::U8 | Self::I8 => 1,
            Self::U16 | Self::I16 => 2,
            Self::U32 | Self::I32 => 4,
            Self::U64 | Self::I64 => 8,
            Self::F32 => 4,
            Self::F64 => 8,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType<'ctx> {
    pub params: Box<[Type<'ctx>]>,
    pub return_: Type<'ctx>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<'ctx> {
    /// void
    Void,
    /// i8
    I8,
    /// i16
    I16,
    /// i32
    I32,
    /// i64
    I64,
    /// isize
    Isize,
    /// u8
    U8,
    /// u16
    U16,
    /// u32
    U32,
    /// u64
    U64,
    /// usize
    Usize,
    /// f32
    F32,
    /// f64
    F64,
    /// an aggregate type
    Aggregate(AggregateID<'ctx>),
    /// an interface type
    Interface(InterfaceID<'ctx>),
    /// a raw pointer
    Pointer(Box<Type<'ctx>>),
    /// a smart pointer
    SmartPointer(Box<Type<'ctx>>),
    /// a function, a safe wrapper around pointer
    Function(Box<FunctionType<'ctx>>),
    /// a fixed size array
    Array(Box<(Type<'ctx>, u64)>),

    /// a future
    Future(Box<Type<'ctx>>),

    SIMDx2(ScalarType),
    SIMDx4(ScalarType),
    SIMDx8(ScalarType),
    SIMDx16(ScalarType),
    SIMDx32(ScalarType),
    SIMDx64(ScalarType),
    SIMDx128(ScalarType),
    SIMDx256(ScalarType),
}

impl<'ctx> Type<'ctx> {
    pub const BOOL: Self = Self::I8;

    /// returns the number of lanes in SIMD types,
    pub fn lanes(&self) -> usize {
        match self {
            Self::SIMDx2(_) => 2,
            Self::SIMDx4(_) => 4,
            Self::SIMDx8(_) => 8,
            Self::SIMDx16(_) => 16,
            Self::SIMDx32(_) => 32,
            Self::SIMDx64(_) => 64,
            Self::SIMDx128(_) => 128,
            Self::SIMDx256(_) => 256,
            _ => 0,
        }
    }

    pub fn size(&self, ctx: &'ctx ()) -> usize {
        match self {
            Self::Void => 0,
            Self::U8 | Self::I8 => 1,
            Self::U16 | Self::I16 => 2,
            Self::U32 | Self::I32 => 4,
            Self::U64 | Self::I64 => 8,

            Self::F32 => 4,
            Self::F64 => 8,

            Self::Usize | Self::Isize | Self::Pointer(_) | Self::SmartPointer(_) | Self::Function(_) => todo!(),
            Self::Aggregate(_) => todo!(),
            Self::Interface(_) => todo!(),
            Self::Future(_) => todo!(),
            Self::Array(a) => a.0.size(ctx) * a.1 as usize,
            Self::SIMDx2(m) => m.size() * 2,
            Self::SIMDx4(m) => m.size() * 4,
            Self::SIMDx8(m) => m.size() * 8,
            Self::SIMDx16(m) => m.size() * 16,
            Self::SIMDx32(m) => m.size() * 32,
            Self::SIMDx64(m) => m.size() * 64,
            Self::SIMDx128(m) => m.size() * 128,
            Self::SIMDx256(m) => m.size() * 256,
        }
    }
}

mod seal {
    pub trait Sealed {}
}

pub trait MarkerType<'ctx>: seal::Sealed + Clone {
    fn to_type(&self) -> Type<'ctx>;
}

pub trait IntoMarkerType<'ctx>{
    type Marker: MarkerType<'ctx>;
    fn into(self) -> Self::Marker;
}

impl<'ctx, T: MarkerType<'ctx>> IntoMarkerType<'ctx> for T{
    type Marker = Self;
    fn into(self) -> Self::Marker {
        return self
    }
}
impl<'ctx> IntoMarkerType<'ctx> for Type<'ctx>{
    type Marker = Auto<'ctx>;
    fn into(self) -> Self::Marker {
        Auto{
            inner: self
        }
    }
}

pub trait FieldedMarkerType<'ctx>: MarkerType<'ctx> {}

pub trait IntMarkerType: seal::Sealed + for<'a> MarkerType<'a> + Default {
    type Type: IntoIntMarkerType;
}
pub trait IntoIntMarkerType: seal::Sealed {
    type Marker: IntMarkerType;
    type Int;
    fn to_i128(self) -> i128;
}
//impl<I:IntMarkerType> IntoIntMarkerType for I{
//    type Marker = I;
//    type Int = I::Type;
//}

pub trait FloatMarkerType: seal::Sealed + for<'a> MarkerType<'a> + Default {
    type Type;
}
pub trait IntoFloatMarkerType: seal::Sealed {
    type Marker: FloatMarkerType;
    type Float;
}
//impl<F:FloatMarkerType> IntoFloatMarkerType for F{
//    type Marker = F;
//    type Float = F::Type;
//}
pub trait ScalarMarkerType: seal::Sealed + for<'a> MarkerType<'a> + Default + Copy {
    const TY: ScalarType;
}
pub trait IntoScalarMarkerType: seal::Sealed {
    type Marker: ScalarMarkerType;
}
pub trait MathMarkerType: seal::Sealed + for<'a> MarkerType<'a> {}
pub trait IntMathMarkerType: MathMarkerType {}
pub trait FloatMathMarkerType: MathMarkerType {}

macro_rules! impl_int_marker {
    ($n:ident, $t:ty) => {
        #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
        pub struct $n;
        impl seal::Sealed for $n {}
        impl<'ctx> MarkerType<'ctx> for $n {
            fn to_type(&self) -> Type<'ctx> {
                return Type::$n;
            }
        }
        impl ScalarMarkerType for $n {
            const TY: ScalarType = ScalarType::$n;
        }
        impl IntoScalarMarkerType for $t {
            type Marker = $n;
        }
        impl MathMarkerType for $n {}
        impl IntMathMarkerType for $n {}
        impl IntMarkerType for $n {
            type Type = $t;
        }
        impl seal::Sealed for $t {}
        impl IntoIntMarkerType for $t {
            type Marker = $n;
            type Int = $t;
            fn to_i128(self) -> i128 {
                self as i128
            }
        }
    };
}
macro_rules! impl_float_marker {
    ($n:ident, $t:ty) => {
        #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
        pub struct $n;
        impl seal::Sealed for $n {}
        impl<'ctx> MarkerType<'ctx> for $n {
            fn to_type(&self) -> Type<'ctx> {
                return Type::$n;
            }
        }
        impl ScalarMarkerType for $n {
            const TY: ScalarType = ScalarType::$n;
        }
        impl IntoScalarMarkerType for $t {
            type Marker = $n;
        }
        impl MathMarkerType for $n {}
        impl FloatMathMarkerType for $n {}
        impl FloatMarkerType for $n {
            type Type = $t;
        }
        impl seal::Sealed for $t {}
        impl IntoFloatMarkerType for $t {
            type Marker = $n;
            type Float = $t;
        }
    };
}
macro_rules! impl_non_scalar_marker {
    ($n:ident, $t:ty) => {
        #[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
        pub struct $n;
        impl seal::Sealed for $n {}
        impl<'ctx> MarkerType<'ctx> for $n {
            fn to_type(&self) -> Type<'ctx> {
                return Type::$n;
            }
        }
        impl MathMarkerType for $n {}
        impl IntMathMarkerType for $n {}
        impl IntMarkerType for $n {
            type Type = $t;
        }
        impl seal::Sealed for $t {}
        impl IntoIntMarkerType for $t {
            type Marker = $n;
            type Int = $t;
            fn to_i128(self) -> i128 {
                self as i128
            }
        }
    };
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Void;

impl seal::Sealed for Void {}

impl<'ctx> MarkerType<'ctx> for Void {
    fn to_type(&self) -> Type<'ctx> {
        Type::Void
    }
}

impl_int_marker!(I8, i8);
impl_int_marker!(I16, i16);
impl_int_marker!(I32, i32);
impl_int_marker!(I64, i64);
impl_int_marker!(U8, u8);
impl_int_marker!(U16, u16);
impl_int_marker!(U32, u32);
impl_int_marker!(U64, u64);

impl_non_scalar_marker!(Usize, usize);
impl_non_scalar_marker!(Isize, isize);

impl_float_marker!(F64, f64);
impl_float_marker!(F32, f32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Function<'ctx, Arg: FunctionArgs<'ctx> = AutoArgs<'ctx>, R: MarkerType<'ctx> = Auto<'ctx>> {
    pub args: Arg,
    pub return_: R,
    pub(crate) _mark: PhantomData<&'ctx ()>,
}

impl<'ctx, Arg: FunctionArgs<'ctx>, R: MarkerType<'ctx>> seal::Sealed for Function<'ctx, Arg, R> {}

impl<'ctx, Arg: FunctionArgs<'ctx>, R: MarkerType<'ctx>> MarkerType<'ctx>
    for Function<'ctx, Arg, R>
{
    fn to_type(&self) -> Type<'ctx> {
        let len = self.args.len();
        let mut params = Vec::with_capacity(len);
        for i in 0..len {
            params.push(self.args.get(i))
        }
        Type::Function(Box::new(FunctionType {
            params: params.into_boxed_slice(),
            return_: self.return_.to_type(),
        }))
    }
}

pub trait FunctionArgs<'ctx>: Clone {
    type ArgValues<'func>:ValueIndex<'ctx, 'func> + ?Sized;
    fn len(&self) -> usize;
    fn get(&self, index: usize) -> Type<'ctx>;
}

pub trait ValueIndex<'ctx, 'func>{
    fn len(&self) -> usize;
    fn get(&self, index: usize) -> Value<'ctx, 'func, Auto<'ctx>>;
}

#[derive(Clone)]
pub struct AutoArgs<'ctx>(Box<[Type<'ctx>]>);

impl<'ctx> FunctionArgs<'ctx> for AutoArgs<'ctx>{
    type ArgValues<'func> = [Value<'ctx, 'func, Auto<'ctx>>];
    fn len(&self) -> usize {
        self.0.len()
    }
    fn get(&self, index: usize) -> Type<'ctx> {
        self.0[index].clone()
    }
}

impl<'ctx, 'func> ValueIndex<'ctx, 'func> for [Value<'ctx, 'func, Auto<'ctx>>]{
    fn len(&self) -> usize {
        self.len()
    }
    fn get(&self, index: usize) -> Value<'ctx, 'func, Auto<'ctx>> {
        self[index].into_auto()
    }
}

impl<'ctx> FunctionArgs<'ctx> for () {
    type ArgValues<'func> = ();
    fn len(&self) -> usize {
        0
    }
    fn get(&self, _index: usize) -> Type<'ctx> {
        unreachable!()
    }
}

impl<'ctx, 'func> ValueIndex<'ctx, 'func> for (){
    fn len(&self) -> usize {
        0
    }
    fn get(&self, _index: usize) -> Value<'ctx, 'func, Auto<'ctx>> {
        unreachable!()
    }
}

impl<'ctx, T: MarkerType<'ctx>> FunctionArgs<'ctx> for T {
    type ArgValues<'func> = Value<'ctx, 'func, T>;
    fn len(&self) -> usize {
        1
    }
    fn get(&self, index: usize) -> Type<'ctx> {
        match index {
            0 => self.to_type(),
            _ => unreachable!(),
        }
    }
}

impl<'ctx, 'func, T:MarkerType<'ctx>> ValueIndex<'ctx, 'func> for Value<'ctx, 'func, T>{
    fn len(&self) -> usize {
        1
    }
    fn get(&self, index: usize) -> Value<'ctx, 'func, Auto<'ctx>> {
        if index == 0{
            self.into_auto()
        } else{
            unreachable!()
        }
    }
}

macro_rules! impl_function_arg {
    ($($ty:ident, $idx:tt),*) => {
        #[allow(unused)]
        impl<'ctx, $($ty:MarkerType<'ctx>),*> FunctionArgs<'ctx> for ($($ty),*){
            type ArgValues<'func> = ($(Value<'ctx, 'func, $ty>),*);
            fn len(&self) -> usize {
                $($idx + 1);*
            }
            fn get(&self, index: usize) -> Type<'ctx> {
                $(
                    if $idx == index{
                        return self.$idx.to_type()
                    }
                )*
                unreachable!()
            }
        }
        #[allow(unused)]
        impl<'ctx, 'func, $($ty:MarkerType<'ctx>),*> ValueIndex<'ctx, 'func> for ($(Value<'ctx, 'func, $ty>),*){
            fn len(&self) -> usize{
                $($idx + 1);*
            }
            fn get(&self, index:usize) -> Value<'ctx, 'func, Auto<'ctx>>{
                $(
                    if $idx == index{
                        return self.$idx.into_auto()
                    }
                )*
                unreachable!()
            }
        }
    };
}

impl_function_arg!(A, 0, B, 1);
impl_function_arg!(A, 0, B, 1, C, 2);
impl_function_arg!(A, 0, B, 1, C, 2, D, 3);
impl_function_arg!(A, 0, B, 1, C, 2, D, 3, E, 4);
impl_function_arg!(A, 0, B, 1, C, 2, D, 3, E, 4, F, 5);
impl_function_arg!(A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6);
impl_function_arg!(A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7);
impl_function_arg!(A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8);
impl_function_arg!(A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9);
impl_function_arg!(A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10);
impl_function_arg!(A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11);
impl_function_arg!(A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15, Q, 16
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15, Q, 16, R, 17
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15, Q, 16, R, 17, S, 18
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15, Q, 16, R, 17, S, 18, T, 19
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15, Q, 16, R, 17, S, 18, T, 19, U, 20
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15, Q, 16, R, 17, S, 18, T, 19, U, 20, V, 21
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15, Q, 16, R, 17, S, 18, T, 19, U, 20, V, 21, W, 22
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15, Q, 16, R, 17, S, 18, T, 19, U, 20, V, 21, W, 22, X, 23
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15, Q, 16, R, 17, S, 18, T, 19, U, 20, V, 21, W, 22, X, 23, Y, 24
);
impl_function_arg!(
    A, 0, B, 1, C, 2, D, 3, E, 4, F, 5, G, 6, H, 7, I, 8, J, 9, K, 10, L, 11, M, 12, N, 13, O, 14,
    P, 15, Q, 16, R, 17, S, 18, T, 19, U, 20, V, 21, W, 22, X, 23, Y, 24, Z, 25
);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Aggregate<'ctx>(pub AggregateID<'ctx>);

impl<'ctx> seal::Sealed for Aggregate<'ctx> {}

impl<'ctx> MarkerType<'ctx> for Aggregate<'ctx> {
    fn to_type(&self) -> Type<'ctx> {
        Type::Aggregate(self.0)
    }
}
impl<'ctx> FieldedMarkerType<'ctx> for Aggregate<'ctx> {}
impl<'ctx> FieldedMarkerType<'ctx> for Pointer<Aggregate<'ctx>> {}
impl<'ctx> FieldedMarkerType<'ctx> for Smart<Aggregate<'ctx>>{}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Interface<'ctx>(pub InterfaceID<'ctx>);

impl<'ctx> seal::Sealed for Interface<'ctx> {}

impl<'ctx> MarkerType<'ctx> for Interface<'ctx> {
    fn to_type(&self) -> Type<'ctx> {
        Type::Interface(self.0)
    }
}
impl<'ctx> FieldedMarkerType<'ctx> for Interface<'ctx> {}
impl<'ctx> FieldedMarkerType<'ctx> for Pointer<Interface<'ctx>>{}
impl<'ctx> FieldedMarkerType<'ctx> for Smart<Interface<'ctx>>{}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Array<T> {
    pub element: T,
    pub length: u64,
}

impl<'ctx, T: MarkerType<'ctx>> seal::Sealed for Array<T> {}

impl<'ctx, T: MarkerType<'ctx>> MarkerType<'ctx> for Array<T> {
    fn to_type(&self) -> Type<'ctx> {
        Type::Array(Box::new((self.element.to_type(), self.length)))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pointer<T> {
    pub pointee: T,
}

impl<'ctx, T> seal::Sealed for Pointer<T> where T: MarkerType<'ctx> {}

impl<'ctx, T> MarkerType<'ctx> for Pointer<T>
where
    T: MarkerType<'ctx>,
{
    fn to_type(&self) -> Type<'ctx> {
        Type::Pointer(Box::new(self.pointee.to_type()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Smart<T> {
    pub pointee: T,
}

impl<'ctx, T> seal::Sealed for Smart<T> where T: MarkerType<'ctx> {}

impl<'ctx, T> MarkerType<'ctx> for Smart<T>
where
    T: MarkerType<'ctx>,
{
    fn to_type(&self) -> Type<'ctx> {
        Type::SmartPointer(Box::new(self.pointee.to_type()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Future<T> {
    pub value: T,
}

impl<'ctx, T: MarkerType<'ctx>> seal::Sealed for Future<T> {}

impl<'ctx, T: MarkerType<'ctx>> MarkerType<'ctx> for Future<T> {
    fn to_type(&self) -> Type<'ctx> {
        Type::Future(Box::new(self.value.to_type()))
    }
}

pub(crate) mod simd {
    pub struct LaneCount<const N: usize>;

    pub trait SupportedLaneCount {}

    impl SupportedLaneCount for LaneCount<2> {}
    impl SupportedLaneCount for LaneCount<4> {}
    impl SupportedLaneCount for LaneCount<8> {}
    impl SupportedLaneCount for LaneCount<16> {}
    impl SupportedLaneCount for LaneCount<32> {}
    impl SupportedLaneCount for LaneCount<64> {}
    impl SupportedLaneCount for LaneCount<128> {}
    impl SupportedLaneCount for LaneCount<256> {}
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct SIMD<T: ScalarMarkerType, const N: usize>(PhantomData<T>)
where
    simd::LaneCount<N>: simd::SupportedLaneCount;

impl<T: ScalarMarkerType, const N: usize> seal::Sealed for SIMD<T, N> where
    simd::LaneCount<N>: simd::SupportedLaneCount
{
}

impl<'ctx, T: ScalarMarkerType, const N: usize> MarkerType<'ctx> for SIMD<T, N>
where
    simd::LaneCount<N>: simd::SupportedLaneCount,
{
    fn to_type(&self) -> Type<'ctx> {
        match N {
            2 => Type::SIMDx2(T::TY),
            4 => Type::SIMDx4(T::TY),
            8 => Type::SIMDx8(T::TY),
            16 => Type::SIMDx16(T::TY),
            32 => Type::SIMDx32(T::TY),
            64 => Type::SIMDx64(T::TY),
            128 => Type::SIMDx128(T::TY),
            256 => Type::SIMDx256(T::TY),
            _ => unreachable!(),
        }
    }
}

impl<T: ScalarMarkerType, const N: usize> MathMarkerType for SIMD<T, N> where
    simd::LaneCount<N>: simd::SupportedLaneCount
{
}

macro_rules! impl_simd_int_math {
    ($t:ty) => {
        impl<const N: usize> IntMathMarkerType for SIMD<$t, N> where
            simd::LaneCount<N>: simd::SupportedLaneCount
        {
        }
    };
}

impl_simd_int_math!(I8);
impl_simd_int_math!(I16);
impl_simd_int_math!(I32);
impl_simd_int_math!(I64);
impl_simd_int_math!(U8);
impl_simd_int_math!(U16);
impl_simd_int_math!(U32);
impl_simd_int_math!(U64);

impl<const N: usize> FloatMathMarkerType for SIMD<F64, N> where
    simd::LaneCount<N>: simd::SupportedLaneCount
{
}
impl<const N: usize> FloatMathMarkerType for SIMD<F32, N> where
    simd::LaneCount<N>: simd::SupportedLaneCount
{
}

#[derive(Clone)]
pub struct Auto<'ctx> {
    pub(crate) inner: Type<'ctx>,
}

impl<'ctx> seal::Sealed for Auto<'ctx> {}
impl<'ctx> MarkerType<'ctx> for Auto<'ctx> {
    fn to_type(&self) -> Type<'ctx> {
        self.inner.clone()
    }
}
