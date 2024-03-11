use core::marker::PhantomData;
use core::ptr::NonNull;

use crate::Type;

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct TypeInfo<'a>(pub(crate) NonNull<TypeInfoHeader>, pub(crate) PhantomData<&'a ()>);

unsafe impl<'a> Sync for TypeInfo<'a> {}
unsafe impl<'a> Send for TypeInfo<'a> {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeId(pub(crate) u32);

#[repr(C)]
pub(crate) struct TypeInfoHeader {
    /// id of the type
    pub(crate) id: TypeId,
    /// byte length of the type info
    pub(crate) length: u32,
    /// size of the structure
    pub(crate) size: u32,
    /// alignment of the structure
    pub(crate) align: u32,
    /// number of fields including extended class
    pub(crate) num_fields: u32,
    /// number of methods including extended class
    pub(crate) num_methods: u32,
    /// whether type extends another type
    pub(crate) has_extends: bool,
    /// this field is only valid when has_extends is true
    pub(crate) extends: TypeId,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct ExtendDescriptor {
    /// id of the type
    pub id: TypeId,
    /// offset of fields
    pub offset: u32,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct FieldDescriptor {
    /// the index of this field
    pub index: u32,
    /// the offset of this field
    pub offset: u32,
    /// the alignment of this field
    pub align: u32,
    /// ty
    pub ty: Type,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct MethodDescriptor<'a>{
    /// the index of this method
    pub index: u32,
    /// the offset of this method
    pub offset: u32,
    /// the return type of this method
    pub return_ty: Type,
    /// the arguments of this method
    pub args: &'a [Type],
}

#[repr(C)]
pub(crate) struct MethodDescriptorHeader{
    /// the index of this method
    pub index: u32,
    /// the offset of this method
    pub offset: u32,
    /// the return type of this method
    pub return_ty: Type,
    /// number of arguments
    pub num_args: u32,
    /// arguments
    pub args: [Type;0],
}

impl<'a> TypeInfo<'a> {
    /// return the unique id of type
    pub fn id(&self) -> TypeId {
        unsafe { self.0.as_ref().id }
    }
    pub fn size(&self) -> usize {
        unsafe { self.0.as_ref().size as usize }
    }
    pub fn alignment(&self) -> usize {
        unsafe { self.0.as_ref().align as usize }
    }
    /// number of fields in type
    pub fn num_fields(&self) -> usize {
        unsafe { (self.0.as_ref()).num_fields as usize }
    }
    /// return the types that type inherites from
    pub fn parent(&self) -> Option<TypeId>{
        unsafe{
            let header = self.0.as_ref();
            if header.has_extends{
                return Some(header.extends)
            }
        }
        return None
    }

    /// iterates fields
    pub fn fields(&self) -> Fields {
        unsafe {
            let ptr = self.0.as_ptr().add(1);

            Fields {
                ptr: ptr as *const u32,
                remain: self.num_fields() as u32,
                mark: PhantomData,
            }
        }
    }

    /// get the field descriptor for the given field
    pub fn get_field_descriptor(&self, name: &str) -> Option<FieldDescriptor> {
        for (k, d) in self.fields() {
            if k == name {
                return Some(d);
            }
        }
        return None;
    }

    /// return true if type has field
    pub fn has_field(&self, name: &str) -> bool {
        for (k, _) in self.fields() {
            if k == name {
                return true;
            }
        }
        return false;
    }

    pub fn methods(&self) -> Methods{
        unsafe{
            let remain = self.0.as_ref().num_methods ;

            // get the offset for method
            let mut fields = self.fields();
            while fields.next().is_some(){}

            Methods{
                ptr: fields.ptr,
                remain: remain,
                mark: PhantomData
            }
        }
    }

    pub fn has_method(&self, name: &str) -> bool{
        for (p, _) in self.methods(){
            if p == name{
                return true
            }
        }
        return false
    }

    pub fn get_method_descriptor(&self, name: &str) -> Option<MethodDescriptor>{
        for (p, m) in self.methods(){
            if p == name{
                return Some(m)
            }
        }
        return None
    }


    /// byte representation of the type info
    pub fn as_bytes(&self) -> &[u8] {
        unsafe {
            core::slice::from_raw_parts(self.0.as_ptr() as _, self.0.as_ref().length as usize)
        }
    }
}

#[derive(Clone)]
pub struct Fields<'a> {
    ptr: *const u32,
    remain: u32,
    mark: PhantomData<&'a ()>,
}

impl<'a> Iterator for Fields<'a> {
    type Item = (&'a str, FieldDescriptor);
    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            if self.remain == 0 {
                return None;
            }

            // read the string len
            let len = self.ptr.read();
            let mut ptr = self.ptr.add(1) as *const u8;

            // get the string
            let s = core::str::from_utf8_unchecked(core::slice::from_raw_parts(ptr, len as usize));
            ptr = ptr.add(len as usize);

            // align pointer
            ptr = (ptr as *const u8).add(0x04 - (s.len() % 0x04)) as _;

            // get the field descriptor
            let f = (ptr as *const FieldDescriptor).read();
            ptr = ptr.add(core::mem::size_of::<FieldDescriptor>());

            // assign new pointer
            self.ptr = ptr as *const u32;
            self.remain -= 1;

            return Some((s, f));
        }
    }
}

#[derive(Clone)]
pub struct Methods<'a>{
    ptr: *const u32,
    remain: u32,
    mark: PhantomData<&'a ()>,
}

impl<'a> Iterator for Methods<'a>{
    type Item = (&'a str, MethodDescriptor<'a>);
    fn next(&mut self) -> Option<Self::Item> {
        if self.remain == 0{
            return None
        }

        unsafe{
            let mut ptr = self.ptr;
            
            // read the string len
            let str_len = ptr.read();
            ptr = ptr.add(1);

            // get the string ptr
            let str_ptr = ptr as *const u8;
            ptr = (ptr as *const u8).add(str_len as usize) as _;

            ptr = (ptr as *const u8).add(0x04 - (str_len as usize % 0x04)) as _;

            // get the header
            let header = (ptr as *const MethodDescriptorHeader).read();
            ptr = (ptr as *const u8).add(core::mem::size_of::<MethodDescriptorHeader>()) as _;

            let num_args = header.num_args;
            let args = ptr as *const Type;

            ptr = (ptr as *const u8).add(core::mem::size_of::<Type>() * num_args as usize) as _;

            self.ptr = ptr;
            self.remain -= 1;

            return Some((
                core::str::from_utf8_unchecked(core::slice::from_raw_parts(str_ptr, str_len as usize)),
                MethodDescriptor{
                    index: header.index,
                    offset: header.offset,
                    return_ty: header.return_ty,
                    args: core::slice::from_raw_parts(args, num_args as usize),
            }))
        }
    }
}