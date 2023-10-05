use core::marker::PhantomData;

#[repr(u8)]
enum TypeEnum{
    Void,
    Pointer,
    Bool,

    I8,
    I16,
    I32,
    I64,
    I128,

    I8x2,
    I8x4,
    I8x8,
    I8x16,
    I8x32,
    I8x64,
    I8x128,
    I8x256,

    I16x2,
    I16x4,
    I16x8,
    I16x16,
    I16x32,
    I16x64,
    I16x128,
    I16x256,
}

pub trait Type: Sized{
    fn size() -> usize{
        core::mem::size_of::<Self>()
    }
}

impl Type for (){}

impl Type for i8{}
impl Type for i16{}
impl Type for i32{}
impl Type for i64{}
impl Type for i128{}
impl Type for f32{}
impl Type for f64{}

impl<T:Type> Type for *mut T{}

impl<const SIZE:usize, T:Type> Type for [T;SIZE]{}


pub trait FloatType: Type{}

impl FloatType for f32{}
impl FloatType for f64{}

pub trait IntegerType: Type{}

impl IntegerType for i8{}
impl IntegerType for i16{}
impl IntegerType for i32{}
impl IntegerType for i64{}
impl IntegerType for i128{}