use std::sync::atomic::{AtomicU8, Ordering};


#[repr(C)]
#[derive(Debug)]
pub struct Flags(AtomicU8);

impl Flags{
    pub const EMPTY: u8 = 0;
    pub const GREY: u8 = 0b00000001;
    pub const BLACK: u8 = 0b00000010;
    pub const ALLOCATED: u8 = 0b00000100;

    pub fn clear(&self){
        self.0.store(0, Ordering::SeqCst);
    }
    pub fn is_white(&self) -> bool{
        self.0.load(Ordering::SeqCst) & (Self::GREY | Self::BLACK) == Self::EMPTY
    }
    pub fn set_white(&self) -> bool{
        let f = self.0.fetch_and(!(Self::BLACK | Self::GREY), Ordering::SeqCst);
        return f  & (Self::GREY | Self::BLACK) == Self::EMPTY
    }
    pub fn is_grey(&self) -> bool{
        self.0.load(Ordering::SeqCst) & Self::GREY == Self::GREY
    }
    pub fn set_grey(&self){
        self.0.fetch_or(Self::GREY, Ordering::SeqCst);
    }
    pub fn is_black(&self) -> bool{
        self.0.load(Ordering::SeqCst) & Self::BLACK == Self::BLACK
    }
    pub fn swap_black(&self, b: bool) -> bool{
        if b{
            let f = self.0.fetch_or(Self::BLACK, Ordering::SeqCst);
            return f & Self::BLACK == Self::BLACK
        } else{
            let f = self.0.fetch_and(!Self::BLACK, Ordering::SeqCst);
            return f & Self::BLACK == Self::BLACK
        }
    }
    pub fn is_allocated(&self) -> bool{
        self.0.load(Ordering::SeqCst) & Self::ALLOCATED == Self::ALLOCATED
    }
    pub fn swap_allocated(&self, b: bool) -> bool{
        if b{
            let f = self.0.fetch_or(Self::ALLOCATED, Ordering::SeqCst);
            return f & Self::ALLOCATED == Self::ALLOCATED
        } else{
            let f = self.0.fetch_and(!Self::ALLOCATED, Ordering::SeqCst);
            return f & Self::ALLOCATED == Self::ALLOCATED
        }
    }
}

#[repr(C)]
pub struct CellHeader{
    pub flags: Flags,
    pub dtor: Option<extern "C" fn(*mut u8)>,
    pub trace: Option<extern "C" fn(*mut u8)>
}

#[repr(C)]
pub struct Cell{
    pub(crate) header: CellHeader,
    pub(crate) payload: [u8;0],
}

impl Cell{
    pub fn trace(&self){
        // set colour to black
        if self.header.flags.swap_black(true){
            // already traced
            return;
        }
        if let Some(t) = &self.header.trace{
            (t)(self.payload.as_ptr() as *mut u8)
        }
    }
}