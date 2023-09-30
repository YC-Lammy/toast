use crate::types::*;

impl JSString {
    /// ECMA 6.1.4.1
    pub fn StringIndexOf(self, search_value: Self, from_index: usize) -> Option<usize> {
        let len = self.len() as i64;
        if search_value.is_empty() && from_index as i64 <= len {
            return Some(from_index);
        }

        if from_index as i64 >= len {
            return None;
        }

        let s = self.as_slice();
        let search_str = search_value.as_slice();

        let re = s[from_index..]
            .windows(search_str.len())
            .position(|chunk| chunk == search_str);

        if let Some(p) = re {
            return Some(p);
        }
        return None;
    }
}
