
use iron_gc::GcPtr;

use super::{JSValue, Any};

#[derive(Clone, Copy)]
#[repr(C)]
pub struct JSString(GcPtr<JSStringInner>);

#[repr(packed)]
pub struct JSStringInner{
    hash: u64,
    len:usize,
    data:[u8;0]
}

impl iron_gc::Trace for JSStringInner{
    fn additional_bytes(&self) -> usize {
        return self.len
    }
    fn trace(&mut self, visitor: &mut iron_gc::Visitor) {}
}

unsafe impl Sync for JSStringInner {}
unsafe impl Send for JSStringInner {}

impl iron_gc::Trace for JSString{
    fn trace(&mut self, visitor: &mut iron_gc::Visitor) {
        visitor.visit(self.0)
    }
}

impl PartialEq for JSString {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl core::hash::Hash for JSString {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.as_slice().hash(state)
    }
}

impl JSValue for JSString{
    fn data_bits(&self) -> u64 {
        self.0.to_raw_ptr() as u64
    }

    fn type_tag(&self) -> u64 {
        Any::STRING_TAG
    }

    fn from_any(any:Any) -> Self {
        let p = any.data();

        unsafe{Self(GcPtr::from_raw_ptr(p as _).unwrap())}
    }
}

impl JSString {
    pub fn new(s:&str) -> Self{
        Self::from_str(s)
    }    

    pub fn hash(&self) -> u64{
        return self.0.hash
    }

    pub fn len(&self) -> usize {
        self.0.len
    }

    pub fn is_empty(&self) -> bool {
        return self.len() == 0;
    }

    pub fn as_slice(&self) -> &'static [u8] {
        unsafe{
            return core::slice::from_raw_parts_mut(&self.0.data as *const _ as *mut u8, self.len())
        }
    }

    pub fn as_str(&self) -> &str{
        unsafe{core::str::from_utf8_unchecked(self.as_slice())}
    }

    pub fn from_str(s: &str) -> Self {
        unsafe{
            // one more byte for null terminator
            let size = s.len() + core::mem::size_of::<JSStringInner>() + 1;

            // malloc
            let mut ptr:GcPtr<JSStringInner> = GcPtr::<u8>::malloc(size).cast();

            // copy string to memory
            core::ptr::copy_nonoverlapping(s.as_bytes().as_ptr(), ptr.data.as_mut_ptr(), s.len());

            // write the null terminator
            (ptr.as_mut_ptr() as *mut u8).add(size - 1).write(0);

            ptr.hash = native_js_common::hash_string(s);

            return Self(ptr)
        }
    }

    /// jsstring is directly compatable with cstring
    pub fn to_c_str(&self) -> *const libc::c_char{
        unsafe{
            return self.as_str().as_ptr() as _
        }
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

    pub fn write_barriar<T:iron_gc::Trace>(&self, other: GcPtr<T>){
        other.write_barriar(self.0);
    }

    pub fn set_uncollectable(&self){
        self.0.set_uncollectable();
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