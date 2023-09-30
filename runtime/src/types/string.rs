
use crate::gc::GcPtr;

use super::{JSValue, Any};

#[derive(Clone, Copy)]
#[repr(C)]
pub enum JSString{
    Empty,
    Alloc(GcPtr<JSStringInner>),
    Constant(&'static str)
}

#[repr(packed)]
pub struct JSStringInner{
    len:usize,
    data:[u8;0]
}

unsafe impl Sync for JSStringInner {}
unsafe impl Send for JSStringInner {}

impl PartialEq for JSString {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Eq for JSString {}

impl core::hash::Hash for JSString {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state)
    }
}

impl JSValue for JSString{
    fn data_bits(&self) -> u64 {
        match self{
            Self::Empty => 0,
            Self::Alloc(a) => a.as_cell() as *const _ as u64,
            Self::Constant(c) => {
                let n = Self::from_str(*c);
                unsafe{core::ptr::write(self as *const Self as *mut Self, n)};
                return n.data_bits();
            }
        }
    }

    fn type_tag(&self) -> u64 {
        Any::STRING_TAG
    }

    fn from_any(any:Any) -> Self {
        let p = any.data();

        if p == 0{
            Self::Empty
        } else{
            Self::Alloc(unsafe{GcPtr::from_cell(p as usize as _)})
        }
    }
}

impl JSString {
    pub fn new(s:&str) -> Self{
        Self::from_str(s)
    }    

    pub fn len(&self) -> usize {
        match self{
            Self::Empty => 0,
            Self::Alloc(p) => p.len,
            Self::Constant(c) => c.len()
        }
    }

    pub fn is_empty(&self) -> bool {
        return self.len() == 0;
    }

    pub fn as_slice(&self) -> &'static [u8] {

        match self{
            Self::Empty => &[],
            Self::Alloc(a) => {
                unsafe{core::slice::from_raw_parts(a.data.as_ptr(), a.len)}
            },
            Self::Constant(c) => c.as_bytes()
        }
    }

    pub fn as_str(&self) -> &str{
        unsafe{core::str::from_utf8_unchecked(self.as_slice())}
    }

    pub fn from_str(s: &str) -> Self {
        if s.len() == 0{
            return Self::Empty
        }

        unsafe{
            let mut ptr:GcPtr<JSStringInner> = GcPtr::<u8>::malloc_array(s.len() + core::mem::size_of::<usize>()).cast();
            core::ptr::copy_nonoverlapping(s.as_bytes().as_ptr(), ptr.data.as_mut_ptr(), s.len());
            return Self::Alloc(ptr)
        }
    }

    pub fn to_c_str(&self) -> &'static core::ffi::CStr{
        unsafe{
            let alloc = libc::malloc(self.len() + 1) as *mut u8;
            core::ptr::copy_nonoverlapping(self.as_slice().as_ptr(), alloc, self.len());

            alloc.add(self.len()).write(0);
            core::ffi::CStr::from_ptr(alloc as *const i8)
        }
    }

    pub fn hash_key_from_utf8(s: &str) -> u64 {
        return cityhasher::hash(s.as_bytes())
    }

    pub fn to_hash_key(&self) -> u64 {
        return cityhasher::hash(self.as_slice())
    }

    pub fn find(&self, pat: Self) -> Option<usize> {
        if pat.len() == 0{
            return Some(0)
        }
        let pat = pat.as_slice();
        self.as_slice()
            .windows(pat.len())
            .position(|chunck| chunck == pat)
    }
}

impl core::fmt::Display for JSString {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(self.as_ref())
    }
}

impl AsRef<str> for JSString{
    fn as_ref(&self) -> &str {
        unsafe{core::str::from_utf8_unchecked(self.as_slice())}
    }
}

#[test]
fn test_string_format(){
    let s = JSString::new("hello world");
    let s2 = JSString::new("hello world");
    assert!(s.len() == 11);
    assert!(s.as_str() == s2.as_str());
}