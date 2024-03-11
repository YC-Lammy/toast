pub mod typeinfo;

use std::{marker::PhantomData, ptr::NonNull};

pub use typeinfo::TypeId;
pub use typeinfo::TypeInfo;



#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Void,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    Pointer,

    I8Array(u32),
    I16Array(u32),
    I32Array(u32),
    I64Array(u32),
    I128Array(u32),
    F32Array(u32),
    F64Array(u32),
    PointerArray(u32),

    Object(typeinfo::TypeId),
    ObjectArray(typeinfo::TypeId, u32),
}

impl Type{
    pub fn size(self) -> usize{
        match self{
            Type::Void => 0,

            Type::I8 => 1,
            Type::I16 => 2,
            Type::I32 => 4,
            Type::I64 => 8,
            Type::I128 => 16,
            Type::F32 => 4,
            Type::F64 => 8,

            Type::I8Array(i) => 1 * i as usize,
            Type::I16Array(i) => 2 * i as usize,
            Type::I32Array(i) => 4 * i as usize,
            Type::I64Array(i) => 8 * i as usize,
            Type::I128Array(i) => 16 * i as usize,
            Type::F32Array(i) => 4 * i as usize,
            Type::F64Array(i) => 8 * i as usize,

            Type::Pointer => 8,
            Type::PointerArray(i) => 8 * i as usize,

            Type::Object(_) => 8,
            Type::ObjectArray(_, i) => 8 * i as usize,
        }
    }

    pub fn alignment(self) -> usize{
        match self {
            Type::Void => 0x1,

            Type::I8 => 1,
            Type::I16 => 2,
            Type::I32 => 4,
            Type::I64 => 8,
            Type::I128 => 16,
            Type::F32 => 4,
            Type::F64 => 8,

            Type::I8Array(_) => 1,
            Type::I16Array(_) => 2,
            Type::I32Array(_) => 4,
            Type::I64Array(_) => 8,
            Type::I128Array(_) => 16,
            Type::F32Array(_) => 4,
            Type::F64Array(_) => 8,

            Type::Pointer => 8,
            Type::PointerArray(_) => 8,

            Type::Object(_) => 8,
            Type::ObjectArray(_, _) => 8
        }
    }
}

pub struct Context {
    info: Vec<u8>,
}

impl Context {
    pub fn new() -> Self {
        Self { info: Vec::new() }
    }

    pub fn type_info_as_bytes(&self) -> &[u8] {
        &self.info
    }

    pub fn get_info(&self, id: TypeId) -> typeinfo::TypeInfo {
        unsafe {
            typeinfo::TypeInfo(
                NonNull::new(self.info.as_ptr().add(id.0 as usize) as *mut _).unwrap_unchecked(),
                PhantomData
            )
        }
    }

    pub fn new_builder(&mut self, extends: Option<TypeId>) -> Builder {
        Builder { 
            context: self,
            extends: extends,
            fields: Vec::new(),
            methods: Vec::new()
        }
    }
}

#[derive(Debug, Clone)]
pub enum BuildError{
    DuplicatedField{
        name: String
    },
    /// the field of inheritance has conflict
    ConflictedFields{
        name: String,
        expected: Type,
        found: Type,
    },
    DuplicatedMethod{
        name: String
    },
    /// the inherited method does not match its type
    ConflictedMethod{
        name: String,
        expected: (Vec<Type>, Type),
        found: (Vec<Type>, Type)
    },
}

pub struct Builder<'a> {
    context: &'a mut Context,
    extends: Option<TypeId>,
    fields: Vec<(String, Type)>,
    methods: Vec<(String, Vec<Type>, Type)>,
}

impl<'a> Builder<'a> {
    pub fn with_field(mut self, name: &str, ty: Type) -> Self {
        self.fields.push((name.into(), ty));
        self
    }
    pub fn with_method(mut self, name: &str, args: &[Type], return_ty: Type) -> Self{
        self.methods.push((name.into(), args.to_vec(), return_ty));
        self
    }
    pub fn build(self) -> Result<TypeId, BuildError> {
        let id = TypeId(self.context.info.len() as u32);

        let mut index = 0;
        let mut offset = 0;
        let mut size = 0;
        let mut length = core::mem::size_of::<typeinfo::TypeInfoHeader>();
        let mut alignment = 0x1;

        let mut extend_fields = Vec::new();
        let mut extend_methods = Vec::new();

        if let Some(id) = &self.extends{
            let info = self.context.get_info(*id);

            for (k, desc) in info.fields(){
                debug_assert!(desc.index == index);

                // length of string
                length += core::mem::size_of::<u32>();
                // string
                length += k.len();
                // padding
                length += 0x04 - (k.len() % 0x04);
                // descriptor
                length += core::mem::size_of::<typeinfo::FieldDescriptor>();
                
                extend_fields.push((k.to_string(), desc));
                
                index += 1;
            }

            for (k, desc) in info.methods(){
                debug_assert!(desc.index == index);

                length += core::mem::size_of::<u32>();
                length += k.len();
                // padding
                length += 0x04 - (k.len() % 0x04);
                // index
                length += core::mem::size_of::<u32>();
                // offset
                length += core::mem::size_of::<u32>();
                // return type
                length += core::mem::size_of::<Type>();
                // number of arguments
                length += core::mem::size_of::<u32>();
                // arguments
                length += core::mem::size_of::<Type>() * desc.args.len();
                
                extend_methods.push((k.to_string(), (desc.index, desc.offset, desc.args.to_vec(), desc.return_ty)));
                
                index += 1;
            }

            offset += info.size() as u32;
            size += info.size();
            alignment = info.alignment();
        }

        let mut fields = Vec::new();
        let mut methods = Vec::new();
        
        // loop through fields
        for (p, ty) in self.fields{
            if let Some((_, desc)) = extend_fields.iter().find(|(n, _)| n == &p){
                if desc.ty != ty{
                    return Err(BuildError::ConflictedFields { 
                        name: p, 
                        expected: desc.ty, 
                        found: ty
                    })
                }
            } else if fields.iter().find(|(n, _)|n==&p).is_some(){
                return Err(BuildError::DuplicatedField { name: p })
            } else{
                // length of name
                length += core::mem::size_of::<u32>();
                // the name
                length += p.len();
                // padding
                length += 0x04 - (p.len() % 0x04);
                // the descriptor
                length += core::mem::size_of::<typeinfo::FieldDescriptor>();
                
                // push descriptor
                fields.push((p, typeinfo::FieldDescriptor{
                    index: index,
                    offset: offset,
                    align: ty.alignment() as u32,
                    ty: ty,
                }));
                
                index += 1;
                offset += ty.size() as u32;
                size += ty.size();
                alignment = alignment.max(ty.alignment());
            };
        };

        // loop through methods
        for (p, args, re) in self.methods{
            // the inherited methods
            if let Some((_, (_, _, margs, mre))) = extend_methods.iter().find(|(n, _)|n == &p){
                if margs != &args || mre != &re{
                    return Err(BuildError::ConflictedMethod { 
                        name: p, 
                        expected: (margs.clone(), mre.clone()), 
                        found: (args, re) 
                    })
                }
            } else if methods.iter().find(|(n, _)|n==&p).is_some(){
                return Err(BuildError::DuplicatedMethod { name: p })
            } else{
                // length of name
                length += core::mem::size_of::<u32>();
                // the name
                length += p.len();
                // padding
                length += 0x04 - (p.len() % 0x04);
                // index
                length += core::mem::size_of::<u32>();
                // offset
                length += core::mem::size_of::<u32>();
                // return type
                length += core::mem::size_of::<Type>();
                // number of arguments
                length += core::mem::size_of::<u32>();
                // arguments
                length += core::mem::size_of::<Type>() * args.len();
                
                methods.push((p, (index, offset, args, re)));
                
                index += 1;
                offset += Type::Pointer.size() as u32;
                size += Type::Pointer.size();
                alignment = alignment.max(Type::Pointer.alignment());
            }
        }

        unsafe{
            // reserve length
            self.context.info.reserve(length);
            self.context.info.set_len(self.context.info.len() + length);

            let mut ptr = self.context.info.as_ptr().add(id.0 as usize) as *mut u8;

            // the header
            let header: [u8;core::mem::size_of::<typeinfo::TypeInfoHeader>()] = core::mem::transmute(
                typeinfo::TypeInfoHeader{
                    id: id,
                    length: length as u32,
                    size: size as u32,
                    align: alignment as u32,
                    num_fields: (fields.len() + extend_fields.len())as u32,
                    num_methods: (methods.len() + extend_methods.len())as u32,
                    has_extends: self.extends.is_some(),
                    extends: self.extends.unwrap_or(TypeId(u32::MAX)),
                }
            );
            // copy header
            core::ptr::copy_nonoverlapping(header.as_ptr(), ptr, header.len());
            ptr = ptr.add(header.len());
            
            // copy inherited fields
            for (name, desc) in extend_fields{
                // copy length of name
                let len = name.len() as u32;
                core::ptr::copy_nonoverlapping(&len as *const u32 as *const u8, ptr, 4);
                ptr = ptr.add(4);

                // copy the name
                core::ptr::copy_nonoverlapping(name.as_ptr(), ptr, name.len());
                ptr = ptr.add(name.len());

                // align the pointer
                ptr = (ptr as *const u8).add(0x04 - (name.len() % 0x04)) as _;

                // copy the descriptor
                core::ptr::copy_nonoverlapping(&desc as *const _ as *const u8, ptr, core::mem::size_of::<typeinfo::FieldDescriptor>());
                ptr = ptr.add(core::mem::size_of::<typeinfo::FieldDescriptor>());
            }

            // copy inherited methods
            for (name, (index, offset, args, re)) in extend_methods{
                // copy length of name
                let len = name.len() as u32;
                core::ptr::copy_nonoverlapping(&len as *const u32 as *const u8, ptr, 4);
                ptr = ptr.add(4);

                // copy the name
                core::ptr::copy_nonoverlapping(name.as_ptr(), ptr, name.len());
                ptr = ptr.add(name.len());

                // align the pointer
                ptr = (ptr as *const u8).add(0x04 - (name.len() % 0x04)) as _;

                let header = typeinfo::MethodDescriptorHeader{
                    index: index,
                    offset: offset,
                    return_ty: re,
                    num_args: args.len() as u32,
                    args: []
                };
                core::ptr::copy_nonoverlapping(&header as *const _ as *const u8, ptr, core::mem::size_of::<typeinfo::MethodDescriptorHeader>());
                ptr = ptr.add(core::mem::size_of::<typeinfo::MethodDescriptorHeader>());

                // copy the arguments
                core::ptr::copy_nonoverlapping(args.as_ptr() as *const u8, ptr, core::mem::size_of::<Type>() * args.len());
                ptr = ptr.add(core::mem::size_of::<Type>() * args.len());
            }

            // copy fields
            for (name, desc) in fields{
                // copy length of name
                let len = name.len() as u32;
                core::ptr::copy_nonoverlapping(&len as *const u32 as *const u8, ptr, 4);
                ptr = ptr.add(4);

                // copy the name
                core::ptr::copy_nonoverlapping(name.as_ptr(), ptr, name.len());
                ptr = ptr.add(name.len());

                // align the pointer
                ptr = (ptr as *const u8).add(0x04 - (name.len() % 0x04)) as _;

                // copy the descriptor
                core::ptr::copy_nonoverlapping(&desc as *const _ as *const u8, ptr, core::mem::size_of::<typeinfo::FieldDescriptor>());
                ptr = ptr.add(core::mem::size_of::<typeinfo::FieldDescriptor>());
            }

            // copy methods
            for (name, (index, offset, args, re)) in methods{
                // copy length of name
                let len = name.len() as u32;
                core::ptr::copy_nonoverlapping(&len as *const u32 as *const u8, ptr, 4);
                ptr = ptr.add(4);

                // copy the name
                core::ptr::copy_nonoverlapping(name.as_ptr(), ptr, name.len());
                ptr = ptr.add(name.len());

                // align the pointer
                ptr = (ptr as *const u8).add(0x04 - (name.len() % 0x04)) as _;

                // copy the header
                let header = typeinfo::MethodDescriptorHeader{
                    index: index,
                    offset: offset,
                    return_ty: re,
                    num_args: args.len() as u32,
                    args: []
                };
                core::ptr::copy_nonoverlapping(&header as *const _ as *const u8, ptr, core::mem::size_of::<typeinfo::MethodDescriptorHeader>());
                ptr = ptr.add(core::mem::size_of::<typeinfo::MethodDescriptorHeader>());

                // copy the arguments
                core::ptr::copy_nonoverlapping(args.as_ptr() as *const u8, ptr, core::mem::size_of::<Type>() * args.len());
                ptr = ptr.add(core::mem::size_of::<Type>() * args.len());
            }
        }

        return Ok(id)
    }
}

pub struct EncodedTypeInfo<'a>(u64, PhantomData<&'a ()>);

impl<'a> EncodedTypeInfo<'a>{
    const PREFIX_VOID: u64 = 0;
    const PREFIX_I8: u64 = 1 << 60;
    const PREFIX_I16: u64 = 2 << 60;
    const PREFIX_I32: u64 = 3 << 60;
    const PREFIX_I64: u64 = 4 << 60;
    const PREFIX_I128: u64 = 6 << 60;
    const PREFIX_F32: u64 = 7 << 60;
    const PREFIX_F64: u64 = 8 << 60;
    const PREFIX_PTR: u64 = 9 << 60;
    const PREFIX_OBJ: u64 = 10 << 60;

    fn from_type(context: &Context, ty: Type) -> EncodedTypeInfo{
        let prefix;
        let mut payload = 0;

        match ty{
            Type::Void => prefix = Self::PREFIX_VOID,
            Type::I8 => prefix = Self::PREFIX_I8,
            Type::I16 => prefix = Self::PREFIX_I16,
            Type::I32 => prefix = Self::PREFIX_I32,
            Type::I64 => prefix = Self::PREFIX_I64,
            Type::I128 => prefix = Self::PREFIX_I128,
            Type::F32 => prefix = Self::PREFIX_F32,
            Type::F64 => prefix = Self::PREFIX_F64,
            Type::Object(id) => {
                let info = context.get_info(id);
                payload = info.0.as_ptr() as u64;
                prefix = Self::PREFIX_OBJ
            },
            Type::Pointer => prefix = Self::PREFIX_PTR,

            Type::I8Array(i) => {
                prefix = Self::PREFIX_I8;
                payload = i as u64;
            }
            Type::I16Array(i) => {
                prefix = Self::PREFIX_I16;
                payload = i as u64;
            }
            Type::I32Array(i) => {
                prefix = Self::PREFIX_I32;
                payload = i as u64;
            }
            Type::I64Array(i) => {
                prefix = Self::PREFIX_I64;
                payload = i as u64;
            }
            Type::I128Array(i) => {
                prefix = Self::PREFIX_I128;
                payload = i as u64;
            }
            Type::F32Array(i) => {
                prefix = Self::PREFIX_F32;
                payload = i as u64;
            }
            Type::F64Array(i) => {
                prefix = Self::PREFIX_F64;
                payload = i as u64;
            }
            Type::PointerArray(i) => {
                prefix = Self::PREFIX_PTR;
                payload = i as u64;
            }
            Type::ObjectArray(id, i) => {
                if i >= 4096{
                    todo!()
                }
                prefix = Self::PREFIX_OBJ;
                let info = context.get_info(id);
                payload = info.0.as_ptr() as u64 | ((i as u16 as u64) << 48);
            }
        };

        todo!()
    }
}

#[test]
fn test(){
    let mut ctx = Context::new();

    let id = ctx.new_builder(None)
    .with_field("hello", Type::I32)
    .with_field("world", Type::F32)
    .with_method("name", &[Type::I8, Type::I128], Type::Void)
    .with_method("mymethod", &[Type::F32Array(2)], Type::I8)
    .build().expect("");

    let info = ctx.get_info(id);
    for (p, d) in info.fields(){
        println!("{}: {:#?}", p, d)
    }

    for (p, d) in info.methods(){
        println!("{}: {:#?}", p, d)
    }
}