use std::sync::Arc;

use crate::structure::any::TsAny;

use super::proto_tree::{ProtoNode, PROTOTREE};

bitflags::bitflags! {
    pub struct PropertyDescriptorFlags: u8{
        const WRITABLE = 0b00000001;
        const CONFIGURABLE = 0b00000010;
        const ENUMERABLE = 0b00000100;
        const IS_GETTER = 0b00001000;
        const IS_SETTER = 0b00010000;
    }
}

bitflags::bitflags! {
    pub struct TsObjectFlags: u32{
        const EXTENSIBLE = 1;

        const IS_FUNCTION = 1 << 1;
        const IS_BOOLEAN = 1 << 2;
        const IS_NUMBER = 1 << 3;
        const IS_BIGINT = 1 << 4;
        const IS_STRING = 1 << 5;
        const IS_DATE = 1 << 6;

        const IS_INT8_ARRAY = 1 << 7;
        const IS_UINT8_ARRAY = 1 << 8;
        const IS_UINT8_CLAMPED_ARRAY = 1 << 9;
        const IS_INT16_ARRAY = 1 << 10;
        const IS_UINT16_ARRAY = 1 << 11;
        const IS_INT32_ARRAY = 1 << 12;
        const IS_UINT32_ARRAY = 1 << 13;
        const IS_INT64_ARRAY = 1 << 14;
        const IS_UINT64_ARRAY = 1 << 15;
        const IS_F16_ARRAY = 1 << 16;
        const IS_F32_ARRAY = 1 << 17;
        const IS_F64_ARRAY = 1 << 18;
        
        const IS_ARRAY_BUFFER = 1 << 23;
        const IS_SHARED_ARRAY_BUFFER = 1 << 24;
        const IS_DATA_VIEW = 1 << 25;

        const IS_MAP = 1 << 19;
        const IS_SET = 1 << 20;
        const IS_WEAK_MAP = 1 << 21;
        const IS_WEAK_SET = 1 << 22;
        
        const IS_WEAK_REF = 1 << 26;

        const IS_PROMISE = 1 << 27;
        const IS_GENERATOR = 1 << 28;
        const IS_ASYNC_GENERATOR = 1 << 29;

        const IS_PROXY = 1 << 30;
    }
}

/// property is packed to reduce footage
#[repr(packed)]
pub struct TsObjectProperty {
    flags: PropertyDescriptorFlags,
    value: TsAny,
}

pub struct TsDynamicObject {
    prototree: Arc<ProtoNode>,
    flags: TsObjectFlags,
    array_length: usize,
    data: Vec<TsObjectProperty>,
}

impl TsDynamicObject {
    pub fn new() -> Self {
        Self {
            prototree: PROTOTREE.root.clone(),
            flags: TsObjectFlags::EXTENSIBLE,
            array_length: 0,
            data: vec![],
        }
    }

    #[inline]
    pub fn get_property(&self, name: &str) -> Option<TsAny> {
        if self.array_length != 0 {
            if let Some(idx) = atoi::atoi::<usize>(name.as_bytes()) {
                if idx < self.array_length {
                    let prop = &self.data[idx];

                    // property is a getter
                    if prop.flags.contains(PropertyDescriptorFlags::IS_GETTER) {
                        // TODO: dynamic object getter
                        todo!()
                    }

                    // return the array element
                    return Some(prop.value);
                }
            }
        }

        if let Some(idx) = self.prototree.search_property(name) {
            let idx = self.array_length + idx;

            let prop = &self.data[idx];

            // property is a getter
            if prop.flags.contains(PropertyDescriptorFlags::IS_GETTER) {
                // TODO: dynamic object getter
                todo!()
            }

            // return the array element
            return Some(prop.value);
        }

        // get the __proto__
        if let Some(proto) = self.data.get(self.array_length) {
            if let Some(obj) = proto.value.as_dynamic_object() {
                unsafe { return obj.as_ref().get_property(name) }
            }
        }

        return None;
    }

    #[inline]
    pub fn set_property(&mut self, name: &str, value: TsAny) -> bool {
        if self.array_length != 0 {
            if let Some(idx) = atoi::atoi::<usize>(name.as_bytes()) {
                if idx < self.array_length {
                    let prop = &mut self.data[idx];

                    // setters are not affected by the writable flag
                    if prop.flags.contains(PropertyDescriptorFlags::IS_SETTER) {
                        // TODO: dynamic object setter
                        todo!();
                    }

                    // not writable
                    if !prop.flags.contains(PropertyDescriptorFlags::WRITABLE) {
                        return false;
                    }

                    // set value
                    prop.value = value;
                    return true;
                }
            }
        }

        // search for property
        if let Some(idx) = self.prototree.search_property(name) {
            let idx = self.array_length + idx;
            let prop = &mut self.data[idx];

            // setters are not affected by the writable flag
            if prop.flags.contains(PropertyDescriptorFlags::IS_SETTER) {
                // TODO: dynamic object setter
                todo!();
            }

            // not writable
            if !prop.flags.contains(PropertyDescriptorFlags::WRITABLE) {
                return false;
            }

            // set value
            prop.value = value;
            return true;
        } else {
            // no property found, extend object

            // object is not extensible
            if !self.flags.contains(TsObjectFlags::EXTENSIBLE) {
                return false;
            }

            // add property to proto tree
            self.prototree = self.prototree.clone().add_property(name);
            let idx = self.array_length + self.prototree.property_index;

            // index should be last
            debug_assert!(idx == self.data.len());

            // push property
            self.data.push(TsObjectProperty {
                flags: PropertyDescriptorFlags::WRITABLE
                    | PropertyDescriptorFlags::CONFIGURABLE
                    | PropertyDescriptorFlags::ENUMERABLE,
                value: value,
            });

            return true;
        };
    }

    #[inline]
    pub fn array_push(&mut self, value: TsAny) -> bool {
        // not extensible
        if !self.flags.contains(TsObjectFlags::EXTENSIBLE) {
            return false;
        }

        let idx = self.array_length;
        self.array_length += 1;

        self.data.insert(
            idx,
            TsObjectProperty {
                flags: PropertyDescriptorFlags::WRITABLE
                    | PropertyDescriptorFlags::CONFIGURABLE
                    | PropertyDescriptorFlags::ENUMERABLE,
                value: value,
            },
        );

        return true;
    }

    /// just get the nth property
    pub fn get_nth_property(&self, index: u32) -> Option<TsAny> {
        if let Some(prop) = self.data.get(index as usize) {
            return Some(prop.value);
        }
        None
    }
}
