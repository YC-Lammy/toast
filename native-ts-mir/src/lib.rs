use std::{sync::Arc, marker::PhantomData};

use parking_lot::RwLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ident(u64);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type{
    /// void
    Void,
    /// bool
    Bool,
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
    /// f32
    F32,
    /// f64
    F64,
    /// an aggregate type
    Aggregate(Arc<AggregateType>),
    /// an interface type
    Intferace(Arc<InterfaceType>),
    /// a function pointer
    FunctionType(Arc<FunctionType>),
    /// a pointer
    Pointer(Box<Type>),
    /// a fixed size array
    Array(Box<(Type, u64)>),

    SIMDx2(MathType),
    SIMDx4(MathType),
    SIMDx8(MathType),
    SIMDx16(MathType),
    SIMDx32(MathType),
    SIMDx64(MathType),
    SIMDx128(MathType),
    SIMDx256(MathType),

    /// a matrix (rows, columns)
    Matrix(MathType, u8, u8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MathType{
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    F32,
    F64,
}

impl MathType{
    pub fn is_signed(&self) -> bool{
        match self{
            Self::U8
            | Self::U16
            | Self::U32
            | Self::U64
            | Self::Usize => false,
            _ => true
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AggregateType{
    pub fields: Vec<(Ident, Type)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceType{
    pub variants: Vec<Arc<AggregateType>>,
    pub fields: Vec<(Ident, Type)>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType{
    pub params: Vec<Type>,
    pub return_: Type
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueID(u64);

impl ValueID{
    fn new() -> Self{
        static ID: core::sync::atomic::AtomicU64 = core::sync::atomic::AtomicU64::new(0);

        Self(ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Value<'block>{
    id: ValueID,
    _mark: PhantomData<&'block ()>
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ICond{
    EQ,
    NE,
    GT,
    GTEQ,
    LT,
    LTEQ,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FCond{
    /// ordered and equal
    OEQ,
    /// ordered and greater than
    OGT,
    /// ordered and greater than or equal
    OGE,
    /// ordered and less than
    OLT,
    /// ordered and less than or equal
    OLE,
    /// ordered and not equal
    ONE,
    /// ordered (no NaN)
    ORD,
    /// unordered or equal
    UEQ,
    /// unordered or greater than
    UGT,
    /// unordered or greater than or equal
    UGE,
    /// unordered or less than
    ULT,
    /// unordered or less than or equal
    ULE,
    /// unordered or nor equal
    UNE,
    /// unordered (either NaN)
    UNO
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ordering{
    Acquire,
    Release,
    AcqRel,
    SeqCst
}

#[derive(Debug, Clone)]
pub struct SwitchCase{
    pub test: i64,
    pub block: Arc<Block>,
    pub block_args: Vec<ValueID>
}

#[derive(Debug, Clone)]
pub struct StackSlot{

}

#[derive(Debug, Clone)]
pub enum MIR{
    Uconst(u128, ValueID),
    Iconst(i128, ValueID),
    Float(f64, ValueID),

    /// negative value
    Neg(ValueID, ValueID),
    Abs(ValueID, ValueID),

    Add(ValueID, ValueID, ValueID),
    Sub(ValueID, ValueID, ValueID),
    Mul(ValueID, ValueID, ValueID),
    Exp(ValueID, ValueID, ValueID),
    Rem(ValueID, ValueID, ValueID),
    Div(ValueID,ValueID, ValueID),
    Shl(ValueID, ValueID),
    Shr(ValueID,ValueID),

    Bitand(ValueID, ValueID, ValueID),
    BitOr(ValueID, ValueID, ValueID),
    Bitxor(ValueID, ValueID, ValueID),
    /// bitnot
    Bitnot(ValueID, ValueID),
    /// bit reverse
    Bitrev(ValueID, ValueID),
    Bitswap(ValueID, ValueID),
    BitOnes(ValueID, ValueID),
    BitLeadingZeros(ValueID, ValueID),
    BitTrailingZeros(ValueID, ValueID),
    /// bitcast a value, both type must have the same size
    Bitcast(ValueID, ValueID),

    /// compare two integers, returns a bool
    Icmp(ICond, ValueID, ValueID, ValueID),
    /// compare two floats, returns a bool
    Fcmp(FCond, ValueID, ValueID, ValueID),

    /// return the minimum value.
    /// when value type is float, if either value is NaN, NaN is returned.
    Min(ValueID, ValueID),
    /// return the maximum value.
    /// when value type is float, if either value is NaN, NaN is returned.
    Max(ValueID, ValueID),
    Select(ValueID, ValueID, ValueID),

    // float operations

    Sqrt(ValueID, ValueID),
    Sin(ValueID, ValueID),
    Cos(ValueID, ValueID),
    Powi(ValueID, ValueID),
    Powf(ValueID, ValueID),
    Floor(ValueID, ValueID),
    Ceil(ValueID, ValueID),
    Round(ValueID, ValueID),

    /// converts int to float
    IntToFloat(ValueID, ValueID),
    /// converts float to int
    FloatToInt(ValueID, ValueID),
    /// converts from one int type to another
    IntCast(ValueID, ValueID),
    /// converts from f64 to f32 or f32 to f64
    FloatCast(ValueID, ValueID),

    /// extract an element from vector
    ExtractElement(ValueID, u16, ValueID),
    /// insert element to vector
    InsertElement(ValueID, u16),

    /// creates an aggregate structure value
    Aggregate(Box<[ValueID]>, ValueID),
    /// converts an aggregate pointer to interface
    Interface(ValueID, ValueID),
    /// extracts a field from either aggregate or interface
    ExtractValue(ValueID, Ident, ValueID),
    /// inserts a value to field to either aggregate or interface
    InsertValue(ValueID, Ident, ValueID),

    /// loads from the stack
    StackLoad(StackSlot, ValueID),
    /// stores to the stack
    StackStore(StackSlot, ValueID),
    /// loads a value from aggregate structure in stackslot
    StackLoadAggregate(StackSlot, Ident, ValueID),
    /// stores a value to aggregate structure in stackslot
    StackStoreAggregate(StackSlot, Ident, ValueID),

    /// get the location of stackslot
    StackPtr(StackSlot, ValueID),

    // (pointer, result)
    /// loads a value from location
    Load(ValueID, ValueID),
    // (pointer, value)
    /// stores a value to location
    Store(ValueID, ValueID),

    /// calculates the pointer to elements with offsets.
    ElementPtr(ValueID, Box<[usize]>),

    AtomicFence(Ordering),
    /// (pointer, cmp, new, sucess ordering, failure ordering, loaded value, sucess)
    /// 
    /// compare exchange
    AtomicCompareExchange(Box<(ValueID, ValueID, ValueID, Ordering, Ordering, ValueID, ValueID)>),

    /// unconditionally branch to a block
    Branch(Arc<Block>, Box<[ValueID]>),
    /// branch if zero
    Brz(Arc<Block>, Box<[ValueID]>),
    /// branch if not zero
    Brnz(Arc<Block>, Box<[ValueID]>),
    Switch(ValueID, Box<[SwitchCase]>),
    /// return a value or void
    Return(Option<ValueID>),
}


#[derive(Debug)]
pub struct Block{
    params: Vec<Type>,
    param_values: Vec<Value<'static>>,
    values: Vec<(ValueID, Type)>,
    instructions: Vec<MIR>,
}

impl Block{
    pub fn new(params: &[Type]) -> Self{
        let mut param_values = Vec::new();
        let mut values = Vec::new();

        for i in 0..params.len(){
            let id = ValueID::new();

            param_values.push(Value { id: id, _mark: PhantomData });
            values.push((id, params[i].clone()));
        }

        Self { 
            params: params.to_vec(), 
            param_values: param_values, 
            values: values,
            instructions: Vec::new()
        }
    }

    pub fn block_params<'block>(&'block self) -> &[Value<'block>]{
        return &self.param_values
    }

    pub fn builder(&mut self) -> BlockInstBuilder{
        BlockInstBuilder { block: self }
    }
}

type Error = String;

pub struct BlockInstBuilder<'a>{
    block: &'a mut Block
}

impl<'block> BlockInstBuilder<'block>{
    pub fn uconst(&mut self, ty: Type, value: u128) -> Result<Value<'block>, Error>{
        let id = ValueID::new();
        self.block.values.push((id, ty));
        self.block.instructions.push(MIR::Uconst(value, id));

        return Ok(Value { id: id, _mark: PhantomData })
    }

    pub fn iconst(&mut self, ty: Type, value: i128) -> Result<Value<'block>, Error>{
        match ty{
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::Isize => {}
            _ => {
                return Err("type of iconst must be integer".into())
            }
        }
        let id = ValueID::new();
        self.block.values.push((id, ty));
        self.block.instructions.push(MIR::Iconst(value, id));

        return Ok(Value { id: id, _mark: PhantomData })
    }

    pub fn fconst(&mut self, ty: Type, value: f64) -> Result<Value<'block>, Error>{
        let id = ValueID::new();
        self.block.values.push((id, ty));
        self.block.instructions.push(MIR::Float(value, id));

        return Ok(Value { id: id, _mark: PhantomData })
    }

    pub fn neg(&mut self, value: Value<'block>) -> Result<Value<'block>, Error>{
        let id = ValueID::new();

        let ty = if let Ok(i) = self.block.values.binary_search_by(|(v, _)|v.cmp(&value.id)){
            self.block.values[i].1.clone()
        } else{
            return Err("value does not belong to block".into())
        };

        match ty{
            Type::I8
            | Type::I16
            | Type::I32
            | Type::I64
            | Type::Isize
            | Type::F32
            | Type::F64 => {},
            | Type::Matrix(ty, _, _) => {
                if !ty.is_signed(){
                    return Err("unint cannot be negative".into())
                }
            }
            _ => {
                return Err("value type must be integer or floating point".into())
            }
        }
        self.block.values.push((id, ty));
        self.block.instructions.push(MIR::Neg(value.id, id));

        return Ok(Value { id: id, _mark: PhantomData })
    }


}