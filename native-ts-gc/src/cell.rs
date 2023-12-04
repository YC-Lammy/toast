
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Flags(u8);

impl Flags{
    pub const EMPTY: Flags = Flags(0);
    pub const GREY: Flags = Flags(0b00000001);
    pub const BLACK: Flags = Flags(0b00000010);
    pub const ALLOCATED: Flags = Flags(0b00000100);

    pub fn is_white(self) -> bool{
        self & (Self::GREY | Self::BLACK) == Self::EMPTY
    }
    pub fn is_grey(self) -> bool{
        self & Self::GREY == Self::GREY
    }
    pub fn is_black(self) -> bool{
        self & Self::BLACK == Self::BLACK
    }
    pub fn is_allocated(self) -> bool{
        self & Self::ALLOCATED == Self::ALLOCATED
    }
}

impl core::ops::BitAnd for Flags{
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}
impl core::ops::BitOr for Flags{
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

#[repr(C)]
pub struct CellHeader{
    pub flags: Flags
}

#[repr(C)]
pub struct Cell{
    pub(crate) header: CellHeader,
    pub(crate) payload: [u8;0],
}