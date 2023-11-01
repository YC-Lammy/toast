
use alloc::string::String;


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

impl Ident{
    pub const fn new() -> Self{
        Ident(String::new())
    }

    pub fn from_str(value: &str) -> Self{
        Self(value.into())
    }

    pub fn as_str(&self) -> &str{
        &self.0
    }

    pub fn chain(&self, segment: &str) -> Self{
        let mut s = self.0.clone();
        s.push_str("::");
        s.push_str(segment);

        return Self(s);
    }

    pub fn chain_ty(&self, ty: &str) -> Self{
        let mut s = self.0.clone();
        s.push_str("::<");
        s.push_str(ty);
        s.push_str(">");

        return Self(s)
    }
}