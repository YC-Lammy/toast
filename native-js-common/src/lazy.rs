use core::sync::atomic::{AtomicBool, Ordering};



pub struct Lazy<T, F = fn()->T> where F:FnOnce()-> T{
    initialised: AtomicBool,
    initialter: Option<F>,
    value: Option<T>,
}

impl<T, F> Lazy<T, F> where F:FnOnce()-> T{
    pub const fn new(init: F) -> Self{
        return Self { 
            initialised: AtomicBool::new(false), 
            initialter: Some(init), 
            value: None
        }
    }

    fn as_mut(&self) -> &mut Self{
        unsafe{(self as *const Self as *mut Self).as_mut().unwrap_unchecked()}
    }

    fn check_init(&self){
        if !self.initialised.swap(true, Ordering::SeqCst){
            unsafe{
                let init = self.as_mut().initialter.take().unwrap_unchecked();
                let v = init();
                self.as_mut().value = Some(v);
            }
        }
    }
}

impl<T, F> core::ops::Deref for Lazy<T, F> where F:FnOnce()->T {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.check_init();
        return unsafe{self.value.as_ref().unwrap_unchecked()}
    }
}

impl<T, F> core::ops::DerefMut for Lazy<T, F> where F:FnOnce()->T {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.check_init();
        return unsafe{self.value.as_mut().unwrap_unchecked()}
    }
}

#[test]
fn test_lazy(){
    let l = Lazy::new(||99.7f64);

    assert!(*l == 99.7f64);
}