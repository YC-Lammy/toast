
use iron_gc::GcPtr;

use super::Any;

#[derive(Clone)]
pub enum Promise {
    Resolved(Any),
    Rejected(Any),
    Waiting(GcPtr<Option<Result<Any, Any>>>),
}

impl Promise {
    pub fn poll(&mut self) -> Option<Result<Any, Any>> {
        match self{
            Self::Rejected(r) => Some(Err(*r)),
            Self::Resolved(r) => Some(Ok(*r)),
            Self::Waiting(rev) => {
                
                if let Some(re) = unsafe{rev.read()}{
                    match re{
                        Ok(r) => {
                            *self = Self::Resolved(r);
                            return Some(Ok(r))
                        }
                        Err(e) => {
                            *self = Self::Rejected(e);
                            return Some(Err(e))
                        }
                    }
                }

                return None
            }
        }
    }
}
