use alloc::string::String;



pub struct Regexp{
    
    has_indices: bool,
    global_search:bool,
    ignore_case: bool,
    multiline: bool,
    dot_all: bool,
    unicode: bool,
    unicode_sets: bool,
    sticky: bool,
}

impl Regexp{
    pub fn new(pattern: &str, flags: &str) -> Result<Self, String>{
        return Ok(Self{
            has_indices: flags.contains('d'),
            global_search: flags.contains('g'),
            ignore_case: flags.contains('i'),
            multiline: flags.contains('m'),
            dot_all: flags.contains('s'),
            unicode: flags.contains('u'),
            unicode_sets: flags.contains('v'),
            sticky: flags.contains('y'),

        })
    }
}