use std::{ops::Deref, ptr::NonNull};


pub struct TsString(NonNull<u8>);

unsafe impl Send for TsString{}
unsafe impl Sync for TsString{}

impl TsString{
    pub fn len(&self) -> usize{
        unsafe{
            let mut i = 0;
            let mut ptr = self.0.as_ptr(); 

            while *ptr != 0{
                i += 1;
                ptr = ptr.add(1);
            }

            return i
        }
    }

    pub fn as_bytes(&self) -> &[u8]{
        unsafe{
            core::slice::from_raw_parts(self.0.as_ptr(), self.len())
        }
    }

    pub fn as_mut_bytes(&mut self) -> &mut [u8]{
        unsafe{
            core::slice::from_raw_parts_mut(self.0.as_ptr(), self.len())
        }
    }

    pub fn as_str(&self) -> &str{
        unsafe{
            core::str::from_utf8_unchecked(core::slice::from_raw_parts(self.0.as_ptr(), self.len()))
        }
    }
}

impl Clone for TsString{
    fn clone(&self) -> Self {
        todo!()
    }
}

impl AsRef<str> for TsString{
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Deref for TsString{
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}