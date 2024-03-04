use core::{ops::{DerefMut, Deref}, fmt::Debug};

use alloc::boxed::Box;


pub struct Rc<T>{
    inner: *const RcInner<T>
}

struct RcInner<T>{
    rc: usize,
    value: T
}

impl<T> Rc<T>{
    pub fn new(value: T) -> Self{
        let inner = Box::leak(Box::new(RcInner{
            rc: 1,
            value
        }));

        return Self { inner }
    }
}

impl<T> Clone for Rc<T>{
    fn clone(&self) -> Self {
        let inner = unsafe{(self.inner as *mut RcInner<T>).as_mut().unwrap()};

        inner.rc += 1;

        return Self{
            inner: self.inner
        }
    }
}

impl<T> Drop for Rc<T>{
    fn drop(&mut self) {
        let inner = unsafe{(self.inner as *mut RcInner<T>).as_mut().unwrap()};
        inner.rc -= 1;

        if inner.rc == 0{
            unsafe{drop(Box::from_raw(self.inner as *mut RcInner<T>))};
        }
    }
}

impl<T> AsRef<T> for Rc<T>{
    fn as_ref(&self) -> &T {
        unsafe{&self.inner.as_ref().unwrap().value}
    }
}

impl<T> Deref for Rc<T>{
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T> DerefMut for Rc<T>{
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

impl<T> AsMut<T> for Rc<T>{
    fn as_mut(&mut self) -> &mut T {
        let inner = unsafe{(self.inner as *mut RcInner<T>).as_mut().unwrap()};
        &mut inner.value
    }
}

impl<T:PartialEq> PartialEq for Rc<T>{
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(&other.as_ref())
    }
}

impl<T:Eq> Eq for Rc<T>{}

impl<T:PartialOrd> PartialOrd for Rc<T>{
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        self.as_ref().partial_cmp(&other.as_ref())
    }
}

impl<T:Ord> Ord for Rc<T>{
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<T:Debug> Debug for Rc<T>{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(format_args!("Rc({:?})", self.as_ref()))
    }
}