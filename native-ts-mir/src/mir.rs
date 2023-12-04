use crate::util::*;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ICond {
    EQ,
    NE,
    GT,
    GTEQ,
    LT,
    LTEQ,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FCond {
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
    UNO,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ordering {
    Acquire,
    Release,
    AcqRel,
    SeqCst,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub test: i64,
    pub block: BlockID,
    pub block_args: Vec<ValueID>,
}

#[derive(Debug, Clone)]
pub enum MIR<'ctx> {
    Uconst(u128, ValueID),
    Iconst(i128, ValueID),
    F64const(f64, ValueID),
    F32const(f32, ValueID),

    /// data stored in raw bytes,
    /// convert back when use
    Vconst(Box<[u8]>, ValueID),

    /// negative value
    Neg(ValueID, ValueID),
    /// absolute value
    Abs(ValueID, ValueID),

    Add(ValueID, ValueID, ValueID),
    Sub(ValueID, ValueID, ValueID),
    Mul(ValueID, ValueID, ValueID),
    Exp(ValueID, ValueID, ValueID),
    Rem(ValueID, ValueID, ValueID),
    Div(ValueID, ValueID, ValueID),
    IShl(ValueID, ValueID, ValueID),
    IShr(ValueID, ValueID, ValueID),

    Bitand(ValueID, ValueID, ValueID),
    BitOr(ValueID, ValueID, ValueID),
    Bitxor(ValueID, ValueID, ValueID),
    /// bitnot
    Bitnot(ValueID, ValueID),
    /// bit reverse
    Bitrev(ValueID, ValueID),
    /// swap the order of bytes
    Bitswap(ValueID, ValueID),
    /// count number of ones
    BitOnes(ValueID, ValueID),
    /// count number of leading zeros
    BitLeadingZeros(ValueID, ValueID),
    /// count number of trailing zeros
    BitTrailingZeros(ValueID, ValueID),
    /// bitcast a value, both type must have the same size
    Bitcast(ValueID, ValueID),

    /// compare two integers, returns a bool
    Icmp(ICond, ValueID, ValueID, ValueID),
    /// compare two floats, returns a bool
    Fcmp(FCond, ValueID, ValueID, ValueID),

    /// return the minimum value.
    /// when value type is float, if either value is NaN, NaN is returned.
    Min(ValueID, ValueID, ValueID),
    /// return the maximum value.
    /// when value type is float, if either value is NaN, NaN is returned.
    Max(ValueID, ValueID, ValueID),
    /// if test value is true, return left side, otherwise right.
    Select(ValueID, ValueID, ValueID, ValueID),
    BitSelect(ValueID, ValueID, ValueID, ValueID),

    // float operations
    Sqrt(ValueID, ValueID),
    Sin(ValueID, ValueID),
    Cos(ValueID, ValueID),
    Powi(ValueID, ValueID, ValueID),
    Powf(ValueID, ValueID, ValueID),
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
    ExtractElement(ValueID, u8, ValueID),
    /// insert element to vector
    InsertElement(ValueID, ValueID, u8, ValueID),

    /// creates an aggregate structure value
    Aggregate(Box<[ValueID]>, ValueID),
    /// converts an aggregate pointer to interface
    Interface(ValueID, ValueID),
    /// extracts a field from either aggregate or interface
    ExtractValue(ValueID, Ident, ValueID),
    /// inserts a value to field to either aggregate or interface
    InsertValue(ValueID, Ident, ValueID),

    /// converts one interface to another
    AggregateToInterface(ValueID, InterfaceID<'ctx>, ValueID),
    /// converts one interface to another
    InterfaceToInterface(ValueID, InterfaceID<'ctx>, ValueID),

    CreateStackSlot(StackSlotID, ValueID),
    /// loads from the stack
    StackLoad(StackSlotID, u64, ValueID),
    /// stores to the stack
    StackStore(StackSlotID, u64, ValueID),

    // (slot, result)
    /// get the location of stackslot
    StackPtr(StackSlotID, ValueID),

    // (pointer, result)
    /// loads a value from location
    Load(ValueID, ValueID),
    // (pointer, value)
    /// stores a value to location
    Store(ValueID, ValueID),

    /// calculates the pointer to elements with offsets.
    ElementPtr(ValueID, Box<[usize]>),

    /// fence
    AtomicFence(Ordering),
    /// (pointer, cmp, new, sucess ordering, failure ordering, loaded value, sucess)
    ///
    /// compare exchange
    AtomicCompareExchange(
        Box<(
            ValueID,
            ValueID,
            ValueID,
            Ordering,
            Ordering,
            ValueID,
            ValueID,
        )>,
    ),

    /// unconditionally branch to a block
    Jump(BlockID),
    /// branch if zero
    Brz(ValueID, BlockID, BlockID),
    /// branch if not zero
    Brnz(ValueID, BlockID, BlockID),
    Switch(ValueID, Box<[SwitchCase]>),
    /// return a value or void
    Return(Option<ValueID>),

    Call {
        id: FunctionID<'ctx>,
        args: Box<[ValueID]>,
        return_: ValueID,
    },
    CallIndirect{
        func: ValueID,
        args: Box<[ValueID]>,
        return_: ValueID,
    }
}
