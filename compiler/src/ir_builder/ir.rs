use swc_atoms::JsWord;

use serde::{Deserialize, Serialize};

use super::{FunctionId, VariableId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct IterId(uuid::Uuid);

impl IterId {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TempId(uuid::Uuid);

impl TempId {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct VarArgId(uuid::Uuid);

impl VarArgId {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ArgListId(uuid::Uuid);

impl ArgListId {
    pub fn new() -> Self {
        Self(uuid::Uuid::new_v4())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum IR {
    Noop,
    Debugger,
    /// create and store a temporary value
    StoreTemp(TempId),
    /// read the temporary value to ACC
    LoadTemp(TempId),
    /// release the memory
    DropTemp(TempId),

    Block {
        label: JsWord,
    },
    EndBlock,
    /// enters an indefinite loop
    Loop {
        label: Option<JsWord>,
    },
    /// end of the loop statement
    EndLoop,
    /// exits the loop
    Break {
        /// the hash of the label
        label: Option<JsWord>,
    },
    /// break if the value in ACC is falsely
    BreakIfFalse,
    /// break if the given iterator is done
    BreakIfIterDone(IterId),
    /// jump to the next loop
    Continue {
        label: Option<JsWord>,
    },

    /// execute following instructions if ACC is true
    If,
    /// same as IR::If, but a fallback 'else' is present
    IfElse,
    /// end of If statment, enter ELSE statment if the corresponding If is IfElse
    EndIf,
    /// end of the Else statment
    EndElse,

    /// a for...in iter iterates through index
    CreateForInIter(IterId),
    /// a for...of iter iterates through values
    CreateForOfIter(IterId),
    /// a for await...of iter iterates through promises
    CreateAsyncIter(IterId),
    /// get the next value from iter
    IterNext(IterId),
    DropIterator(IterId),

    /// return the value in ACC
    Return,
    /// throw the value in ACC as an exception
    Throw,

    /// enter a catch context
    TryCatch,
    /// end of try block,
    ///
    /// triggers the catch blcok if an error occours,
    /// load the error into ACC
    EndTry,
    /// end of catch block
    EndCatch,
    /// end of finalizer
    EndTryCatchFinalizer,

    /// declares and allocate for a variable if not already allocated
    DeclareVar(VariableId),
    /// writes to a variable
    WriteVar(VariableId),
    /// reads from variable to ACC
    ReadVar(VariableId),

    /// loads the value 'undefined' to ACC
    LoadUndefined,
    /// loads the value 'null' to ACC
    LoadNull,
    /// loads the number to ACC
    LoadNumber(f64),
    /// loads the number as int to ACC
    LoadInt(i32),
    LoadBigInt(i64),
    /// loads the string to the ACC
    LoadString(JsWord),
    LoadBool(bool),

    LoadTpl {
        quasis: Box<[JsWord]>,
        args_len: usize,
        args: ArgListId,
    },

    /// initialise a function object
    InitFunction(FunctionId),

    CreateArgList(ArgListId),
    PushArg(ArgListId),

    /// function: ACC, arg_len: usize
    ///
    /// call the function dynamically
    Call {
        this: TempId,
        arg_len: usize,
        args: ArgListId,
        maybe_static: Option<FunctionId>,
    },
    /// function: ACC, args_array: TempId
    ///
    /// call the function with an array as arguments
    CallVarArgs {
        this: TempId,
        args_array: TempId
    },
    /// call a function statically
    ///
    /// read 'arg_len' numbers of stack slots for arguments
    CallStatic {
        func_id: FunctionId,
        arg_len: usize,
        args: ArgListId,
    },
    /// read the nth param
    ReadParam(usize),
    /// read the remaining params as array
    ReadRemainParams {
        starting_from: usize,
    },

    /// call the constructor dynamically
    New {
        arg_len: usize,
        args: ArgListId,
    },

    /// creates an empty object
    CreateObject,

    /// write field to object using hash as key
    WriteField {
        object: TempId,
        key: JsWord,
    },
    /// obj: ACC, hash: u64
    /// read field from object using hash as key
    ///
    /// the object will be pushed to the *OBJECT* register,
    /// the Index will be pushed to the *INDEX* register for further operations.
    ReadField {
        key: JsWord,
    },
    /// obj: ACC index: usize
    ///
    /// read field from object using the index as key
    ReadIndex {
        index: usize,
    },
    /// obj: TempId, index:usize, value: ACC
    ///
    /// write field to object using index as key
    WriteIndex {
        object: TempId,
        index: usize,
    },
    /// obj: TempId, propname: ACC
    ///
    /// caculate the hash from propname, caculated hash will be stored to *INDEX* and object to *OBJECT*
    ReadComputed {
        obj: TempId,
    },
    /// obj: TempId, propname: Stack, value: ACC
    WriteComputed {
        obj: TempId,
        propname: TempId,
    },
    /// the array is stored in temp
    ///
    /// the value to be push is read from ACC
    ObjectPush {
        array: TempId,
    },

    /// create a regex using runtime specific code
    CreateRegex {
        pattern: JsWord,
        flag: JsWord,
    },

    ReadSuper,
    ReadThis,

    /// compares the ACC with top of the stack,
    ///
    /// pops the value from the stack
    EqEq(TempId),
    /// compares the ACC with top of the stack,
    ///
    /// pops the value from the stack
    EqEqEq(TempId),
    NotEq(TempId),
    NotEqEq(TempId),
    /// a: ACC1, b: ACC0
    ///
    /// adds the ACC and stack value together,
    /// pops from the stack
    Add(TempId),
    Sub(TempId),
    Mul(TempId),
    Div(TempId),
    Mod(TempId),
    LShift(TempId),
    RShift(TempId),
    ZeroFillRShift(TempId),
    /// &&
    And(TempId),
    Or(TempId),
    BitAnd(TempId),
    BitOr(TempId),
    BitXor(TempId),
    Exp(TempId),
    Gt(TempId),
    GtEq(TempId),
    Lt(TempId),
    LtEq(TempId),

    /// a in b
    In(TempId),
    InstanceOf(TempId),
    /// a ?? b
    ///
    /// if a is null or undefined, return b
    Nullish(TempId),
    /// a: on stack, b: ACC
    ///
    /// return b if a is undefine, else b
    NonUndefine(TempId),

    /// waits for a promise to be fulfilled.
    Await,
    /// yields from an async function
    Yield,

    /// !
    Bang,
    /// delete
    Delete,
    /// -
    Minus,
    /// +
    Plus,
    /// ~
    Tilde,
    /// typeof
    TypeOf,
}
