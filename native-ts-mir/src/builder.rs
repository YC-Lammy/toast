use std::cell::RefCell;
use std::marker::PhantomData;

use types::{Auto, AutoArgs, Enum, FunctionType, Future, Generator, PointerMarkerType, Smart, ValueIndex};

use crate::function::{BlockDesc, Function};
use crate::mir::{FCond, ICond, Ordering, MIR};
use crate::types::simd::{LaneCount, SupportedLaneCount};
use crate::types::{
    Aggregate, FieldedMarkerType, FloatMarkerType, FloatMathMarkerType, IntMarkerType,
    IntMathMarkerType, Interface, IntoFloatMarkerType, IntoIntMarkerType, IntoScalarMarkerType,
    MarkerType, MathMarkerType, Pointer, ScalarMarkerType, Type, I32, I8, SIMD,
};
use crate::util::{AggregateID, BlockID, FunctionID, Ident, InterfaceID, StackSlotID, ValueID};
pub use crate::Value;
use crate::{types, Context};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Block<'func> {
    id: BlockID,
    _mark: PhantomData<&'func ()>,
}

#[derive(Clone)]
pub struct StackSlot<'ctx, 'func, T: MarkerType<'ctx>> {
    id: StackSlotID,
    ty: T,
    _mark: PhantomData<(&'func (), &'ctx ())>,
}

pub struct Builder<'ctx, 'func>
where
    'ctx: 'func,
{
    ctx: &'ctx Context,
    func: RefCell<&'func mut Function<'ctx>>,
    current_block: RefCell<Option<&'func mut BlockDesc<'ctx>>>,
}

impl<'ctx, 'func> Builder<'ctx, 'func>
where
    'ctx: 'func,
{
    pub fn new(ctx: &'ctx Context, func: &'func mut Function<'ctx>) -> Self{
        Self { 
            ctx: ctx, 
            func: RefCell::new(func), 
            current_block: RefCell::new(None)
        }
    }
    pub fn create_block(&self) -> Block<'func> {
        let id = BlockID::new();
        self.func.borrow_mut().blocks.push(BlockDesc::new(id, &[]));

        return Block {
            id,
            _mark: PhantomData,
        };
    }

    pub fn switch_to_block(&self, block: Block<'func>) {
        let mut f = self
            .func
            .borrow_mut();
        let block: &mut BlockDesc = 
        f
            .blocks
            .iter_mut()
            .rev()
            .find(|b| b.id == block.id)
            .expect("Trying to get instruction builder without declaring block");

        self.current_block.replace(unsafe { core::mem::transmute(block) });
    }

    pub fn global_function(&self, func_id: FunctionID<'ctx>) -> Value<'ctx, 'func, types::Function<'ctx, AutoArgs<'ctx>, Auto<'ctx>>>{
        let f = &self.ctx.functions[func_id.id];
        let params = f.ty.params.clone();
        let mut return_ty = f.ty.return_.clone();

        if f.is_async{
            return_ty = Type::Future(Box::new(return_ty));
        }

        if let Some(gen) = &f.is_generator{
            return_ty = Type::Generator(Box::new((
                if f.is_async{
                   Type::Future(Box::new(gen.yield_type.clone())) 
                } else{
                    gen.yield_type.clone()
                }, 
                gen.resume_type.clone(), 
                return_ty
            )));
        }

        let id = ValueID::new();
        self.func.borrow_mut().map_ssa_func.push((func_id, id));

        return Value { 
            id, 
            ty: types::Function{
                args: AutoArgs(params),
                return_: Auto { inner: return_ty.clone(),
                },
                _mark: PhantomData
            }, _mark: PhantomData 
        }
    }

    /// # panic
    pub fn inst<'builder>(&'builder mut self) -> InstBuilder<'ctx, 'func, 'builder> {
        if self.current_block.get_mut().is_none(){
            panic!("missing entry block")
        }

        InstBuilder {
            builder: self,
            block: unsafe{core::mem::transmute_copy(&self.current_block)},
            _mark: PhantomData,
        }
    }
}
pub struct InstBuilder<'ctx, 'func, 'builder>
where
    'ctx: 'func,
    'func: 'builder,
{
    builder: &'builder Builder<'ctx, 'func>,
    block: RefCell<&'builder mut crate::function::BlockDesc<'ctx>>,
    _mark: PhantomData<&'func ()>,
}

impl<'ctx, 'func, 'builder> InstBuilder<'ctx, 'func, 'builder>
where
    'ctx: 'func,
{
    fn new_ssa(&self, _ty: Type<'ctx>) -> ValueID{
        ValueID::new()
    }

    /// read from param
    pub fn param(&self, index: usize) -> Option<Value<'ctx, 'func, Auto<'ctx>>>{
        if let Some(ty) = self.builder.func.borrow().params.get(index){
            let ty = ty.clone();
            let id = self.new_ssa(ty.clone());

            self.block.borrow_mut().inst.push(MIR::ReadParam(index, id));

            return Some(Value { 
                id: id, 
                ty: Auto { inner: ty }, 
                _mark: PhantomData 
            })
        }
        return None;
    }

    /// an integer constant
    pub fn iconst<I: IntoIntMarkerType>(&self, value: I) -> Value<'ctx, 'func, I::Marker> {
        let id = self.new_ssa(I::Marker::default().to_type());
        self.block.borrow_mut().inst.push(MIR::Iconst(value.to_i128(), id));

        return Value {
            id: id,
            ty: I::Marker::default(),
            _mark: PhantomData,
        };
    }

    /// a floating point constant
    pub fn fconst<F: IntoFloatMarkerType>(&self, value: F) -> Value<'ctx, 'func, F::Marker> {
        let id = self.new_ssa(F::Marker::default().to_type());

        if core::mem::size_of::<F>() == 8 {
            let v = unsafe { *(&value as *const F as *const f64) };
            self.block.borrow_mut().inst.push(MIR::F64const(v, id));
        } else {
            debug_assert!(core::mem::size_of::<F>() == 4);
            let v = unsafe { *(&value as *const F as *const f32) };
            self.block.borrow_mut().inst.push(MIR::F32const(v, id));
        }
        return Value {
            id,
            ty: F::Marker::default(),
            _mark: PhantomData,
        };
    }

    /// an simd constant
    pub fn vconst<T: IntoScalarMarkerType, const N: usize>(
        &self,
        values: [T; N],
    ) -> Value<'ctx, 'func, SIMD<T::Marker, N>>
    where
        crate::types::simd::LaneCount<N>: crate::types::simd::SupportedLaneCount,
    {
        let id = self.new_ssa(SIMD::<T::Marker, N>::default().to_type());
        unsafe {
            let layout = alloc::alloc::Layout::for_value(&values);
            let ptr = alloc::alloc::alloc(layout);
            core::ptr::copy_nonoverlapping(
                &values as *const [T; N] as *const u8,
                ptr,
                layout.size(),
            );
            let slice = core::slice::from_raw_parts_mut(ptr, layout.size());
            let data = Box::from_raw(slice);
            self.block.borrow_mut().inst.push(MIR::Vconst(data, id));

            return Value {
                id,
                ty: SIMD::<T::Marker, N>::default(),
                _mark: PhantomData,
            };
        };
    }

    /// negative value
    pub fn neg<T: MathMarkerType>(
        &self,
        value: Value<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, T> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Neg(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }

    /// absolute value
    pub fn abs<T: MathMarkerType>(
        &self,
        value: Value<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, T> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Abs(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }

    pub fn add<T: MathMarkerType>(
        &self,
        a: Value<'ctx, 'func, T>,
        b: Value<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, T> {
        let id = self.new_ssa(a.ty.to_type());

        self.block.borrow_mut().inst.push(MIR::Add(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    pub fn sub<T: MathMarkerType>(
        &self,
        a: Value<'ctx, 'func, T>,
        b: Value<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, T> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Sub(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }

    /// multiply
    pub fn mul<T: MathMarkerType>(
        &self,
        a: Value<'ctx, 'func, T>,
        b: Value<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, T> {
        let id = self.new_ssa(a.ty.to_type());

        self.block.borrow_mut().inst.push(MIR::Mul(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }

    /// exponent
    pub fn exp<T: MathMarkerType>(
        &self,
        a: Value<'ctx, 'func, T>,
        b: Value<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, T> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Exp(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }

    /// remainder
    pub fn rem<T: MathMarkerType>(
        &self,
        a: Value<'ctx, 'func, T>,
        b: Value<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, T> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Rem(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    /// division
    pub fn div<T: MathMarkerType>(
        &self,
        a: Value<'ctx, 'func, T>,
        b: Value<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, T> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Div(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    /// shift left
    pub fn shl<I: IntMathMarkerType>(
        &self,
        a: Value<'ctx, 'func, I>,
        b: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Shl(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    /// shift right
    pub fn shr<I: IntMathMarkerType>(
        &self,
        a: Value<'ctx, 'func, I>,
        b: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Shr(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    /// bitwise and
    pub fn bitand<I: IntMathMarkerType>(
        &self,
        a: Value<'ctx, 'func, I>,
        b: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Bitand(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    /// bitwise or
    pub fn bitor<I: IntMathMarkerType>(
        &self,
        a: Value<'ctx, 'func, I>,
        b: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::BitOr(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    /// bitwise xor
    pub fn bitxor<I: IntMathMarkerType>(
        &self,
        a: Value<'ctx, 'func, I>,
        b: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Bitxor(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    /// bitwise not
    pub fn bitnot<I: IntMathMarkerType>(
        &self,
        value: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Bitnot(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// bitwise reverse
    pub fn bitrev<I: IntMathMarkerType>(
        &self,
        value: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Bitrev(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// bitwise swap
    pub fn bitswap<I: IntMathMarkerType>(
        &self,
        value: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Bitswap(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// count one bits
    pub fn count_ones<I: IntMathMarkerType>(
        &self,
        value: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::BitOnes(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// count leading zero bits
    pub fn leading_zeros<I: IntMathMarkerType>(
        &self,
        value: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::BitLeadingZeros(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// count trailing zero bits
    pub fn trailing_zeros<I: IntMathMarkerType>(
        &self,
        value: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::BitTrailingZeros(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// cast a type to another
    pub fn bitcast<T: MarkerType<'ctx>, U: MarkerType<'ctx>>(
        &self,
        value: Value<'ctx, 'func, T>,
        ty: U,
    ) -> Value<'ctx, 'func, U> {
        let id = self.new_ssa(ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Bitcast(value.id, id));

        return Value {
            id,
            ty: ty,
            _mark: PhantomData,
        };
    }

    pub fn icmp<I: IntMathMarkerType>(
        &self,
        cond: ICond,
        a: Value<'ctx, 'func, I>,
        b: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Icmp(cond, a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    pub fn fcmp<F: FloatMathMarkerType>(
        &self,
        cond: FCond,
        a: Value<'ctx, 'func, F>,
        b: Value<'ctx, 'func, F>,
    ) -> Value<'ctx, 'func, F> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Fcmp(cond, a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }

    pub fn min<I: MathMarkerType>(
        &self,
        a: Value<'ctx, 'func, I>,
        b: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Min(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    pub fn max<I: MathMarkerType>(
        &self,
        a: Value<'ctx, 'func, I>,
        b: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Max(a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    pub fn select<I: IntMathMarkerType>(
        &self,
        test: Value<'ctx, 'func, I8>,
        a: Value<'ctx, 'func, I>,
        b: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Select(test.id, a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    /// bitwise select
    pub fn bitselect<I: IntMathMarkerType>(
        &self,
        test: Value<'ctx, 'func, I>,
        a: Value<'ctx, 'func, I>,
        b: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, I> {
        let id = self.new_ssa(a.ty.to_type());
        self.block.borrow_mut().inst
            .push(MIR::BitSelect(test.id, a.id, b.id, id));

        return Value {
            id,
            ty: a.ty,
            _mark: PhantomData,
        };
    }
    /// sqroot
    pub fn sqrt<F: FloatMathMarkerType>(
        &self,
        value: Value<'ctx, 'func, F>,
    ) -> Value<'ctx, 'func, F> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Sqrt(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// sin in radiant
    pub fn sin<F: FloatMathMarkerType>(
        &self,
        value: Value<'ctx, 'func, F>,
    ) -> Value<'ctx, 'func, F> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Sin(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// cos in radiant
    pub fn cos<F: FloatMathMarkerType>(
        &self,
        value: Value<'ctx, 'func, F>,
    ) -> Value<'ctx, 'func, F> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Cos(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// power to integer
    pub fn powi<F: FloatMarkerType>(
        &self,
        value: Value<'ctx, 'func, F>,
        exponent: Value<'ctx, 'func, I32>,
    ) -> Value<'ctx, 'func, F> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Powi(value.id, exponent.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// power to float
    pub fn powf<F: FloatMathMarkerType>(
        &self,
        value: Value<'ctx, 'func, F>,
        exponent: Value<'ctx, 'func, F>,
    ) -> Value<'ctx, 'func, F> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Powf(value.id, exponent.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// floor
    pub fn floor<F: FloatMarkerType>(
        &self,
        value: Value<'ctx, 'func, F>,
    ) -> Value<'ctx, 'func, F> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Floor(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// ceil
    pub fn ceil<F: FloatMarkerType>(
        &self,
        value: Value<'ctx, 'func, F>,
    ) -> Value<'ctx, 'func, F> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Ceil(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    /// round
    pub fn round<F: FloatMarkerType>(
        &self,
        value: Value<'ctx, 'func, F>,
    ) -> Value<'ctx, 'func, F> {
        let id = self.new_ssa(value.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::Round(value.id, id));

        return Value {
            id,
            ty: value.ty,
            _mark: PhantomData,
        };
    }
    pub fn int_to_float<F: IntoFloatMarkerType, I: IntMarkerType>(
        &self,
        value: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, F::Marker> {
        let id = self.new_ssa(F::Marker::default().to_type());
        self.block.borrow_mut().inst.push(MIR::IntToFloat(value.id, id));

        return Value {
            id,
            ty: F::Marker::default(),
            _mark: PhantomData,
        };
    }
    pub fn float_to_int<I: IntoIntMarkerType, F: FloatMarkerType>(
        &self,
        value: Value<'ctx, 'func, F>,
    ) -> Value<'ctx, 'func, I::Marker> {
        let id = self.new_ssa(I::Marker::default().to_type());
        self.block.borrow_mut().inst.push(MIR::FloatToInt(value.id, id));

        return Value {
            id,
            ty: I::Marker::default(),
            _mark: PhantomData,
        };
    }
    /// extend or reduce bits to cast integer to another
    pub fn int_cast<U: IntoIntMarkerType, I: IntMarkerType>(
        &self,
        value: Value<'ctx, 'func, I>,
    ) -> Value<'ctx, 'func, U::Marker> {
        let id = self.new_ssa(U::Marker::default().to_type());
        self.block.borrow_mut().inst.push(MIR::IntCast(value.id, id));

        return Value {
            id,
            ty: U::Marker::default(),
            _mark: PhantomData,
        };
    }
    /// cast between f64 and f32
    pub fn float_cast<U: IntoFloatMarkerType, F: FloatMarkerType>(
        &self,
        value: Value<'ctx, 'func, F>,
    ) -> Value<'ctx, 'func, U::Marker> {
        let id = self.new_ssa(U::Marker::default().to_type());
        self.block.borrow_mut().inst.push(MIR::FloatCast(value.id, id));

        return Value {
            id,
            ty: U::Marker::default(),
            _mark: PhantomData,
        };
    }
    /// extract an element from vector
    pub fn extract_element<T: ScalarMarkerType, const N: usize>(
        &self,
        vector: Value<'ctx, 'func, SIMD<T, N>>,
        index: u8,
    ) -> Value<'ctx, 'func, T>
    where
        LaneCount<N>: SupportedLaneCount,
    {
        if index as usize >= N {
            panic!("index larger then lanes")
        }
        let id = self.new_ssa(T::default().to_type());
        self.block.borrow_mut().inst
            .push(MIR::ExtractElement(vector.id, index as _, id));

        return Value {
            id,
            ty: T::default(),
            _mark: PhantomData,
        };
    }
    /// insert an element to vector
    pub fn insert_element<T: ScalarMarkerType, const N: usize>(
        &self,
        vector: Value<'ctx, 'func, SIMD<T, N>>,
        value: Value<'ctx, 'func, T>,
        index: u8,
    ) -> Value<'ctx, 'func, SIMD<T, N>>
    where
        LaneCount<N>: SupportedLaneCount,
    {
        if index as usize >= N {
            panic!("index larger then lanes")
        }
        let id = self.new_ssa(vector.ty.to_type());
        self.block.borrow_mut().inst
            .push(MIR::InsertElement(vector.id, value.id, index as _, id));

        return Value {
            id,
            ty: SIMD::<T, N>::default(),
            _mark: PhantomData,
        };
    }
    /// construct an aggregate type
    pub fn aggregate(
        &self,
        ty: AggregateID<'ctx>,
        values: &[Value<'ctx, 'func, Auto<'ctx>>],
    ) -> Value<'ctx, 'func, Aggregate<'ctx>> {
        let agg = self.builder.ctx.get_aggregate(ty);

        if values.len() != agg.fields.len() {
            panic!("invalid arguments")
        }

        for (i, (_key, ty)) in agg.fields.iter().enumerate() {
            if &values[i].ty.inner != ty {
                panic!("mismatch type")
            }
        }
        let id = self.new_ssa(Type::Aggregate(ty));

        self.block.borrow_mut().inst
            .push(MIR::Aggregate(values.iter().map(|v| v.id).collect(), id));

        return Value {
            id,
            ty: Aggregate(ty),
            _mark: PhantomData,
        };
    }
    pub fn aggregate_to_interface(
        &self,
        value: Value<'ctx, 'func, Smart<Aggregate<'ctx>>>,
        iface: InterfaceID<'ctx>,
    ) -> Value<'ctx, 'func, Interface<'ctx>> {
        let agg = self.builder.ctx.get_aggregate(value.ty.pointee.0);
        let interface = self.builder.ctx.get_interface(iface);

        if agg.fields.len() < interface.fields.len() {
            panic!("aggregate type does not match interface")
        }

        for (ident, ty) in &interface.fields {
            if agg
                .fields
                .iter()
                .find(|(k, t)| k == ident && t == ty)
                .is_none()
            {
                panic!("aggregate type does not match interface")
            }
        }
        let id = self.new_ssa(Type::Interface(iface));
        self.block.borrow_mut().inst
            .push(MIR::AggregateToInterface(value.id, iface, id));

        return Value {
            id: id,
            ty: Interface(iface),
            _mark: PhantomData,
        };
    }
    pub fn interface_to_interface(
        &self,
        value: Value<'ctx, 'func, Interface<'ctx>>,
        iface: InterfaceID<'ctx>,
    ) -> Value<'ctx, 'func, Interface<'ctx>> {
        // same interface, no need to map.
        if value.ty.0 == iface {
            return value;
        }

        let iface1 = self.builder.ctx.get_interface(value.ty.0);
        let iface2 = self.builder.ctx.get_interface(iface);

        for (key, ty) in &iface2.fields {
            if !iface1
                .fields
                .iter()
                .find(|(id, t)| id == key && t == ty)
                .is_some()
            {
                panic!("interface mismatch")
            }
        }

        let id = self.new_ssa(Type::Interface(iface));

        // insert instruction
        self.block.borrow_mut().inst
            .push(MIR::InterfaceToInterface(value.id, iface, id));

        return Value {
            id,
            ty: Interface(iface),
            _mark: PhantomData,
        };
    }

    /// extract a field value from any fielded types.
    /// Accepts aggregate, interface, pointer to aggregate or pointer to interface.
    pub fn extract_value<T: FieldedMarkerType<'ctx>>(
        &self,
        target: Value<'ctx, 'func, T>,
        field: Ident,
    ) -> Value<'ctx, 'func, Auto<'ctx>> {
        let ty = match target.ty.to_type() {
            Type::Aggregate(id) => {
                if let Some((_, ty)) = self
                    .builder
                    .ctx
                    .get_aggregate(id)
                    .fields
                    .iter()
                    .find(|(key, _)| key == &field)
                {
                    ty.clone()
                } else {
                    panic!("aggregate has no field")
                }
            }
            Type::Interface(id) => {
                if let Some((_, ty)) = self
                    .builder
                    .ctx
                    .get_interface(id)
                    .fields
                    .iter()
                    .find(|(key, _)| key == &field)
                {
                    ty.clone()
                } else {
                    panic!("interface has no field")
                }
            }
            Type::SmartPointer(p) | Type::Pointer(p) => match p.as_ref() {
                Type::Aggregate(id) => {
                    if let Some((_, ty)) = self
                        .builder
                        .ctx
                        .get_aggregate(*id)
                        .fields
                        .iter()
                        .find(|(key, _)| key == &field)
                    {
                        ty.clone()
                    } else {
                        panic!("aggregate has no field")
                    }
                }
                Type::Interface(id) => {
                    if let Some((_, ty)) = self
                        .builder
                        .ctx
                        .get_interface(*id)
                        .fields
                        .iter()
                        .find(|(key, _)| key == &field)
                    {
                        ty.clone()
                    } else {
                        panic!("interface has no field")
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        // the lifetime of type is phanom and can be safely transmuted
        let ty: Type<'_> = unsafe { core::mem::transmute(ty.clone())};
        
        let id = self.new_ssa(ty.clone());

        // insert instruction
        self.block.borrow_mut().inst
            .push(MIR::ExtractValue(target.id, field, id));

        return Value {
            id,
            ty: Auto {
                inner: ty,
            },
            _mark: PhantomData,
        };
    }

    /// inserts a value to a field
    /// Accepts aggregate, interface, pointer to aggregate or pointer to interface.
    ///
    /// if garbage collection is enabled, this will be lowered to a call to write barrier
    pub fn insert_value<T: FieldedMarkerType<'ctx>, V: MarkerType<'ctx>>(
        &self,
        target: Value<'ctx, 'func, T>,
        field: Ident,
        value: Value<'ctx, 'func, V>,
    ) {
        let ty = match target.ty.to_type() {
            Type::Aggregate(id) => {
                if let Some((_, ty)) = self
                    .builder
                    .ctx
                    .get_aggregate(id)
                    .fields
                    .iter()
                    .find(|(key, _)| key == &field)
                {
                    ty.clone()
                } else {
                    panic!("aggregate has no field")
                }
            }
            Type::Interface(id) => {
                if let Some((_, ty)) = self
                    .builder
                    .ctx
                    .get_interface(id)
                    .fields
                    .iter()
                    .find(|(key, _)| key == &field)
                {
                    ty.clone()
                } else {
                    panic!("interface has no field")
                }
            }
            Type::SmartPointer(p) | Type::Pointer(p) => match p.as_ref() {
                Type::Aggregate(id) => {
                    if let Some((_, ty)) = self
                        .builder
                        .ctx
                        .get_aggregate(*id)
                        .fields
                        .iter()
                        .find(|(key, _)| key == &field)
                    {
                        ty.clone()
                    } else {
                        panic!("aggregate has no field")
                    }
                }
                Type::Interface(id) => {
                    if let Some((_, ty)) = self
                        .builder
                        .ctx
                        .get_interface(*id)
                        .fields
                        .iter()
                        .find(|(key, _)| key == &field)
                    {
                        ty.clone()
                    } else {
                        panic!("interface has no field")
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        if value.ty.to_type() != ty {
            panic!("type not match")
        }

        self.block.borrow_mut().inst
            .push(MIR::InsertValue(target.id, field, value.id));

        return;
    }

    /// allocate a new stack slot
    pub fn create_stack_slot<T: MarkerType<'ctx>>(
        &self,
        initialiser: Value<'ctx, 'func, T>,
    ) -> StackSlot<'ctx, 'func, T> {
        let mut f = self.builder.func.borrow_mut();
        let id = f.stackslots.len();
        f.stackslots.push(initialiser.ty.to_type());

        drop(f);

        self.block.borrow_mut().inst
            .push(MIR::CreateStackSlot(StackSlotID(id), initialiser.id));

        StackSlot {
            id: StackSlotID(id),
            ty: initialiser.ty,
            _mark: PhantomData,
        }
    }

    /// load value from the stack slot
    pub fn stack_load<T: MarkerType<'ctx>>(
        &self,
        slot: StackSlot<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, T> {
        let id = self.new_ssa(slot.ty.to_type());
        self.block.borrow_mut().inst.push(MIR::StackLoad(slot.id, 0, id));

        return Value {
            id,
            ty: slot.ty.clone(),
            _mark: PhantomData,
        };
    }
    /// write value to the stack slot
    pub fn stack_store<T: MarkerType<'ctx>>(
        &self,
        slot: StackSlot<'ctx, 'func, T>,
        value: Value<'ctx, 'func, T>,
    ) {
        self.block.borrow_mut().inst.push(MIR::StackStore(slot.id, 0, value.id));
    }

    /// retrieve pointer to a stack slot
    pub fn stack_pointer<T: MarkerType<'ctx>>(
        &self,
        slot: StackSlot<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, Pointer<T>> {
        let id = self.new_ssa(Type::Pointer(Box::new(slot.ty.to_type())));
        self.block.borrow_mut().inst.push(MIR::StackPtr(slot.id, id));

        return Value {
            id,
            ty: Pointer {
                pointee: slot.ty.clone(),
            },
            _mark: PhantomData,
        };
    }

    pub fn load<T: MarkerType<'ctx>, P: PointerMarkerType<'ctx, T>>(
        &self,
        ptr: Value<'ctx, 'func, P>,
    ) -> Value<'ctx, 'func, T> {
        let id = self.new_ssa(ptr.ty.pointee().to_type());
        self.block.borrow_mut().inst.push(MIR::Load(ptr.id, id));

        return Value {
            id,
            ty: ptr.ty.pointee().clone(),
            _mark: PhantomData,
        };
    }

    /// stores a value
    pub fn store<T: MarkerType<'ctx>, P: PointerMarkerType<'ctx, T>>(
        &self,
        ptr: Value<'ctx, 'func, P>,
        value: Value<'ctx, 'func, T>,
    ) {
        self.block.borrow_mut().inst.push(MIR::Store(ptr.id, value.id));
    }

    /// memory fence operation
    pub fn fence(&self, ordering: Ordering) {
        self.block.borrow_mut().inst.push(MIR::AtomicFence(ordering));
    }
    /// returns the loaded value and success
    pub fn compare_exchange<T: IntMarkerType>(
        &self,
        ptr: Value<'ctx, 'func, Pointer<T>>,
        cmp: Value<'ctx, 'func, T>,
        new: Value<'ctx, 'func, T>,
        success_order: Ordering,
        failure_order: Ordering,
    ) -> (Value<'ctx, 'func, T>, Value<'ctx, 'func, I8>) {
        let loaded_id = self.new_ssa(T::default().to_type());
        let success_id = self.new_ssa(Type::I8);

        self.block.borrow_mut().inst.push(MIR::AtomicCompareExchange(Box::new((
            ptr.id,
            cmp.id,
            new.id,
            success_order,
            failure_order,
            loaded_id,
            success_id,
        ))));

        return (
            Value {
                id: loaded_id,
                ty: T::default(),
                _mark: PhantomData,
            },
            Value {
                id: success_id,
                ty: I8,
                _mark: PhantomData,
            },
        );
    }

    /// unconditional jump
    pub fn jump(&self, block: Block<'func>) {
        self.block.borrow_mut().inst.push(MIR::Jump(block.id))
    }

    /// branch if zero
    pub fn brz<I: IntMarkerType>(
        &self,
        test: Value<'ctx, 'func, I>,
        then: Block<'ctx>,
        else_: Block<'ctx>,
    ) {
        self.block.borrow_mut().inst.push(MIR::Brz(test.id, then.id, else_.id));
    }

    /// branch if non zero
    pub fn brnz<I: IntMarkerType>(
        &self,
        test: Value<'ctx, 'func, I>,
        then: Block<'ctx>,
        else_: Block<'ctx>,
    ) {
        self.block.borrow_mut().inst.push(MIR::Brnz(test.id, then.id, else_.id));
    }

    /// returns from a function
    pub fn return_<T: MarkerType<'ctx>>(&self, value: Option<Value<'ctx, 'func, T>>) {
        self.block.borrow_mut().inst.push(MIR::Return(value.map(|v| v.id)));
    }

    /// calls a function indirectly
    /// 
    /// # panic
    pub fn call_indirect<Arg: types::FunctionArgs<'ctx>, R: MarkerType<'ctx>>(
        &self,
        func: Value<'ctx, 'func, types::Function<'ctx, Arg, R>>,
        args: &Arg::ArgValues<'func>,
    ) -> Value<'ctx, 'func, R> {
        let args_len = args.len();
        let ty_len = func.ty.args.len();

        if args_len != ty_len {
            panic!("arguments not match")
        };

        let mut arg_values = Vec::new();

        for i in 0..ty_len {
            let arg = args.get(i);
            if arg.ty.inner != func.ty.args.get(i) {
                panic!("argument type not match")
            }
            arg_values.push(arg.id);
        }

        let id = self.new_ssa(func.ty.return_.to_type());

        self.block.borrow_mut().inst.push(MIR::CallIndirect {
            func: func.id,
            args: arg_values.into_boxed_slice(),
            return_: id,
        });
        return Value {
            id,
            ty: func.ty.return_,
            _mark: PhantomData,
        };
    }

    /// this function calls a function directly.
    /// the return value has auto type, user must cast the value before use.
    /// 
    /// # panic
    pub fn call_direct(
        &self,
        id: FunctionID<'ctx>,
        args: &[Value<'ctx, 'func, Auto<'ctx>>],
    ) -> Value<'ctx, 'func, Auto<'ctx>> {
        let ty: &FunctionType<'ctx> =
            unsafe { core::mem::transmute(self.builder.ctx.get_function_type(id)) };

        if ty.params.len() != args.len() {
            panic!("number of arguments not match")
        }
        for (i, t) in ty.params.iter().enumerate() {
            if &args[i].ty.inner != t {
                panic!("argument type not match")
            }
        }

        let return_id = self.new_ssa(ty.return_.clone());

        self.block.borrow_mut().inst.push(MIR::Call {
            id: FunctionID {
                id: id.id,
                _mark: PhantomData,
            },
            args: args.iter().map(|v| v.id).collect(),
            return_: return_id,
        });

        return Value {
            id: return_id,
            ty: Auto {
                inner: ty.return_.clone(),
            },
            _mark: PhantomData,
        };
    }

    /// allocate a smart pointer.
    ///
    /// smart pointers must be stored in a stack slot.
    /// Its SSA values should not be passed around.
    /// Instead, load the smart pointer from stackslot every time value is accessed.
    pub fn malloc<T: MarkerType<'ctx>>(
        &self,
        value: Value<'ctx, 'func, T>,
    ) -> Value<'ctx, 'func, Smart<T>> {
        let id = self.new_ssa(value.ty.to_type());

        self.block.borrow_mut().inst.push(MIR::Malloc(value.id, id));

        return Value {
            id,
            ty: Smart { pointee: value.ty },
            _mark: PhantomData,
        };
    }

    /// frees an allocation
    /// 
    /// if GC is choosen, a safepoint is inserted.
    /// if Arc is choosen, this function does nothing.
    pub fn free<T: MarkerType<'ctx>>(&self, ptr: Value<'ctx, 'func, Smart<T>>){
        self.block.borrow_mut().inst.push(MIR::Free(ptr.id));
    }

    pub fn await_<T: MarkerType<'ctx>>(
        &self,
        future: Value<'ctx, 'func, Future<T>>,
    ) -> Value<'ctx, 'func, T> {
        if !self.builder.func.borrow().is_async{
            panic!("await in non async function")
        }
        let id = self.new_ssa(future.ty.value.to_type());

        self.block.borrow_mut().inst.push(MIR::AsyncAwait(future.id, id));

        return Value {
            id,
            ty: future.ty.value,
            _mark: PhantomData,
        };
    }

    /// yields from a generator
    /// 
    /// all the previous SSA values would be invalidated after this point.
    /// Any value wanting to live across generator boundaries must be stored in stackslot
    pub fn yield_<T: MarkerType<'ctx>>(&self, value: Value<'ctx, 'func, T>) -> Value<'ctx, 'func, Auto<'ctx>>{
        if let Some(desc) = &self.builder.func.borrow().is_generator{
            let resume_ty = desc.resume_type.clone();

            let id = self.new_ssa(resume_ty.clone());

            self.block.borrow_mut().inst.push(MIR::Yield(value.id, id));

            return Value { 
                id, 
                ty: Auto { inner: resume_ty }, 
                _mark: PhantomData 
            }
        } else{
            panic!("yield in non generator function")
        }
    }

    /// resume a generator
    pub fn generator_resume<Y: MarkerType<'ctx>, RE: MarkerType<'ctx>, R: MarkerType<'ctx>, >(&self, generator: Value<'ctx, 'func, Generator<Y, RE, R>>, resume: Value<'ctx, 'func, RE>) -> Value<'ctx, 'func, Enum<'ctx, (Y, R)>>{
        let ty = Type::Enum(Box::new([generator.ty.yield_.to_type(), generator.ty.return_.to_type()]));
        let id = self.new_ssa(ty);

        self.block.borrow_mut().inst.push(MIR::GeneratorNext(generator.id, resume.id, id));

        return Value { 
            id: id, 
            ty: Enum { 
                variants: (generator.ty.yield_, generator.ty.return_), 
                _mark: PhantomData 
            }, 
            _mark: PhantomData
        }
    }
}
