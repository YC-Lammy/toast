

use crate::types::Type;
use crate::Value;

pub trait Params{
    type Values<'block>: ValueList<'block>;
    fn len() -> usize;
}

impl Params for (){
    type Values<'block> = ();
    fn len() -> usize {
        0
    }
}

pub trait ValueList<'block>{
    fn to_ids(&self) -> Vec<u64>;
}

impl<'block> ValueList<'block> for (){
    fn to_ids(&self) -> Vec<u64> {
        return Vec::new()
    }
}
impl<'block, const SIZE:usize, T:Type> ValueList<'block> for [Value<'block, T>;SIZE]{
    fn to_ids(&self) -> Vec<u64> {
        return self.iter().map(|v|v.id).collect()
    }
}

impl<'block, A:Type> ValueList<'block> for Value<'block, A>{
    fn to_ids(&self) -> Vec<u64> {
        vec![self.id]
    }
}
impl<'block, A:Type, B:Type> ValueList<'block> for (Value<'block, A>, Value<'block, B>){
    fn to_ids(&self) -> Vec<u64> {
        vec![self.0.id, self.1.id]
    }
}
impl<'block, A:Type, B:Type, C:Type> ValueList<'block> for (Value<'block, A>, Value<'block, B>, Value<'block, C>){
    fn to_ids(&self) -> Vec<u64> {
        vec![self.0.id, self.1.id, self.2.id]
    }
}