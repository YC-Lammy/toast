use std::borrow::Cow;

use crate::ast::*;

pub fn parse(
    input: &[char],
    unicode_mode: bool,
) -> Result<Pattern, SyntexError> {
    let mut parser = Parser{
        input: input,
        cursor: 0,
        unicode_mode: unicode_mode,
        capture_group_count: 0,
        non_capturing_groups: 0,
        named_capture_groups: Default::default(),
        has_look_behind: false,
    };

    parser.parse_pattern(unicode_mode, true)
}

pub enum SyntexError {
    Expected{
        expected: char,
        offset: usize
    },
    Msg{
        msg: Cow<'static, str>,
        offset: usize
    }
}

impl core::fmt::Display for SyntexError{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self{
            Self::Expected { expected, offset } => f.write_fmt(format_args!("expecting '{}' at offset: {}.", expected, offset)),
            Self::Msg { msg, offset } => f.write_fmt(format_args!("error: {}, at offset: {}", msg, offset))
        }
        
    }
}

impl SyntexError {
    pub fn new(expected: char, offset: usize) -> SyntexError {
        SyntexError::Expected { expected, offset }
    }

    pub fn msg<S:Into<Cow<'static, str>>>(msg: S, offset: usize) -> SyntexError{
        SyntexError::Msg { msg: msg.into(), offset: offset }
    }
}

type PResult<T> = Result<T, SyntexError>;

pub struct Parser<'a> {
    input: &'a [char],
    cursor: usize,
    unicode_mode: bool,

    capture_group_count: usize,
    non_capturing_groups: usize,
    named_capture_groups: Vec<(usize, Box<[char]>)>,

    has_look_behind: bool,
}

#[allow(non_snake_case)]
impl<'a> Parser<'a> {
    fn increment_cusor(&mut self) {
        self.cursor += 1;
    }

    /// increment cursor if matches
    fn match_char(&mut self, ch: char) -> bool {
        if self.cursor == self.input.len() {
            return false;
        }

        if self.input[self.cursor] == ch {
            self.cursor += 1;
            return true;
        }

        return false;
    }

    fn match_range<R: core::ops::RangeBounds<char>>(&mut self, range: R) -> bool {
        if self.cursor == self.input.len() {
            return false;
        }

        let c = self.input[self.cursor];
        if range.contains(&c) {
            self.cursor += 1;
            return true;
        }

        return false;
    }

    fn peek(&self) -> Option<char> {
        if self.cursor == self.input.len() {
            return None;
        }

        return Some(self.input[self.cursor]);
    }

    fn lookbehind(&self) -> Option<char>{
        if self.cursor == 0{
            return None
        }
        return Some(self.input[self.cursor - 1])
    }

    pub fn parse_pattern(&mut self,unicode_mode: bool, N: bool) -> PResult<Pattern> {
        let d = self.parse_disjunction(unicode_mode, N)?;
        Ok(Pattern { 
            disjunction: d,
            capture_groups: self.capture_group_count,
        })
    }

    pub fn parse_disjunction(&mut self, unicode_mode: bool, N: bool) -> PResult<Disjunction> {
        let mut v = Vec::new();

        while self.cursor < self.input.len() {
            v.push(self.parse_alternative(unicode_mode, N)?);

            if self.cursor >= self.input.len(){
                break;
            }

            if !self.match_char('|') {
                return Err(SyntexError::new('|', self.cursor));
            };
        }

        return Ok(Disjunction { alternatives: v });
    }

    pub fn parse_alternative(&mut self, unicode_mode: bool, N: bool) -> PResult<Alternative> {
        let mut terms = Vec::new();

        while self.cursor < self.input.len() {
            if self.peek() == Some('|'){
                break;
            }
            let term = self.parse_term(unicode_mode, N)?;
            terms.push(term);
        }

        return Ok(Alternative { terms: terms });
    }

    pub fn parse_term(&mut self, unicode_mode: bool, N: bool) -> PResult<Term> {
        if let Ok(a) = self.parse_assertion(unicode_mode, N) {
            return Ok(Term::Assertion(a));
        } else {
            let atom = self.parse_atom(unicode_mode, N)?;

            let quantifier = self.parse_quantifier(N)?;
            return Ok(Term::Atom {
                atom: atom,
                quantifier: quantifier,
            });
        }
    }

    pub fn parse_assertion(&mut self, unicode_mode: bool, N: bool) -> PResult<Assertion> {
        let old_cusor = self.cursor;

        if self.match_char('^') {
            return Ok(Assertion::Beginning);
        }

        if self.match_char('$') {
            return Ok(Assertion::End);
        }

        if self.match_char('\\') && (self.peek() == Some('b') || self.peek() == Some('B')) {
            if self.match_char('b') {
                return Ok(Assertion::WordBoundry);
            }

            if self.match_char('B') {
                return Ok(Assertion::NonWordBoundry);
            }
        }

        // (?= disjunction ) (?! disjunction) (?<= disjunction ) (?<! disjunction)
        if self.match_char('(') && self.peek() == Some('?') {
            self.increment_cusor();

            if self.match_char('=') {
                let d = self.parse_disjunction(unicode_mode, N)?;

                if self.match_char(')') {
                    return Ok(Assertion::LookAhead(d));
                }
            }

            if self.match_char('!') {
                let d = self.parse_disjunction(unicode_mode, N)?;

                if self.match_char(')') {
                    return Ok(Assertion::NegativeLookahead(d));
                }
            }

            if self.match_char('<') && (self.peek() == Some('=') || self.peek() == Some('!')) {
                if self.match_char('=') {
                    let d = self.parse_disjunction(unicode_mode, N)?;

                    if self.match_char(')') {
                        self.has_look_behind = true;
                        return Ok(Assertion::LookBehind(d));
                    }
                }

                if self.match_char('!') {
                    let d = self.parse_disjunction(unicode_mode, N)?;

                    if self.match_char(')') {
                        self.has_look_behind = true;
                        return Ok(Assertion::NegativeLookBehind(d));
                    }
                }
            }

            self.cursor = old_cusor;

            return Err(SyntexError::msg("invalid assertion.", self.cursor));
        }

        // restore the cursor
        self.cursor = old_cusor;

        return Err(SyntexError::new('^', self.cursor));
    }

    pub fn parse_quantifier(&mut self, _N: bool) -> PResult<Option<Quantifier>> {
        let old_cursor = self.cursor;

        if self.match_char('*') {
            let greedy = self.match_char('?');
            return Ok(Some(Quantifier::Star { greedy }));
        }

        if self.match_char('+') {
            let greedy = self.match_char('?');
            return Ok(Some(Quantifier::RepeatAtleast { n: 1, greedy }));
        }

        if self.match_char('?') {
            let greedy = self.match_char('?');
            return Ok(Some(Quantifier::RepeatLimited { n: 0, m: 1, greedy }));
        }

        if self.match_char('{') {
            // repeat fixed n times
            if let Some(n) = self.parse_integer() {
                // close
                if self.match_char('}') {
                    // greedy
                    let greedy = self.match_char('?');
                    return Ok(Some(Quantifier::Repeat { n: n, greedy }));

                } else if self.match_char(',') {
                    // limited repeats
                    if let Some(m) = self.parse_integer() {
                        // close
                        if self.match_char('}') {
                            // greedy
                            let greedy = self.match_char('?');

                            if n > m{
                                self.cursor = old_cursor;
                                return Err(SyntexError::msg("numbers out of order in {} quantifier", self.cursor))
                            }
                            return Ok(Some(Quantifier::RepeatLimited { n, m, greedy }));
                        }
                    } else {
                        // at leat n repeats

                        // close
                        if self.match_char('}') {
                            // greedy
                            let greedy = self.match_char('?');
                            return Ok(Some(Quantifier::RepeatAtleast { n, greedy }));
                        }
                    }
                }
            }

            self.cursor = old_cursor;
            return Err(SyntexError::msg("expected numbers in {} quantifier", self.cursor))
        }

        self.cursor = old_cursor;
        return Ok(None);
    }

    pub fn parse_atom(&mut self, unicode_mode: bool, N: bool) -> PResult<Atom> {
        let old_cursor = self.cursor;

        // pattern character excludes syntax characters
        if let Some(c) = self.peek() {
            if c != '^'
                && c != '$'
                && c != '\\'
                && c != '.'
                && c != '*'
                && c != '+'
                && c != '?'
                && c != '('
                && c != ')'
                && c != '['
                && c != ']'
                && c != '{'
                && c != '}'
                && c != '|'
            {
                self.increment_cusor();
                return Ok(Atom::PatternCharacter(c))
            }
        }

        // any character
        if self.match_char('.'){
            return Ok(Atom::Character(Character::Any))
        }

        // atom escape
        if self.match_char('\\'){
            // back reference
            if let Some(c) = self.peek(){
                if '1' <= c && c <= '9'{
                    self.increment_cusor();
                    let index = c.to_digit(10).unwrap();
                    return Ok(Atom::BackReferenceIndex { index })
                }
            }

            // character class escape
            if let Ok(c) = self.parse_character_class_escape(unicode_mode){
                return Ok(Atom::CharacterClass(c))
            }

            if let Ok(c) = self.parse_character_escape(unicode_mode){
                return Ok(Atom::Character(c))
            }

            // named back reference
            if N{
                if self.match_char('k'){
                    let name = self.parse_group_name()?;

                    return Ok(Atom::BackReferenceNamed { name: name })
                }
            }

            return Err(SyntexError::msg("invalide escape sequence.", self.cursor));
        }

        // class range
        if self.match_char('['){

            // negative class range
            let is_negative = self.match_char('^');
            
            let mut ranges = Vec::new();

            while self.cursor < self.input.len(){
                match self.parse_class_range(unicode_mode){
                    Ok(r) => ranges.push(r),
                    Err(e) => {

                        // break if we reach closing
                        if self.peek() == Some(']'){
                            break;
                        } else{
                            return Err(e)
                        }

                    }
                }
            }
            
            if !self.match_char(']'){
                self.cursor = old_cursor;
                return Err(SyntexError::new(']', self.cursor))
            };

            return Ok(Atom::ClassRange(
                CharacterClassRanges { 
                    is_negative: is_negative, 
                    ranges: ranges
                }
            ))
        };

        // a group
        if self.match_char('('){
            // named group or non-capturing group
            if self.match_char('?'){

                // named group
                if self.match_char('<'){
                    // group name
                    let name = self.parse_group_name()?;

                    if !self.match_char('>'){
                        self.cursor = old_cursor;
                        return Err(SyntexError::new('>', self.cursor))
                    };

                    let d = self.parse_disjunction(unicode_mode, N)?;

                    // closing group
                    if !self.match_char(')'){
                        self.cursor = old_cursor;
                        return Err(SyntexError::new(')', self.cursor))
                    };

                    let index = self.capture_group_count;
                    self.capture_group_count += 1;
                    self.named_capture_groups.push((index, name.clone()));

                    return Ok(Atom::CapturingGroup { 
                        index: index as u32, 
                        name: Some(name.into()), 
                        disjunction: d
                    });
                }

                // non_capturing group
                if self.match_char(':'){
                    let d = self.parse_disjunction(unicode_mode, N)?;

                    // closing group
                    if !self.match_char(')'){
                        self.cursor = old_cursor;
                        return Err(SyntexError::new(')', self.cursor))
                    };

                    let index = self.capture_group_count;
                    self.capture_group_count += 1;
                    self.non_capturing_groups += 1;

                    return Ok(Atom::NonCapturingGroup { 
                        index: index as u32, 
                        disjunction: d
                    })
                }

                // named group opening
                return Err(SyntexError::new('<', self.cursor))

            } else{
                // non named group
                let d = self.parse_disjunction(unicode_mode, N)?;

                // closing group
                if self.match_char(')'){
                    let index = self.capture_group_count;
                    self.capture_group_count += 1;

                    return Ok(Atom::CapturingGroup { 
                        index: index as u32, 
                        name: None, 
                        disjunction: d
                    })
                }

                return Err(SyntexError::new(')', self.cursor))
            }
        };

        self.cursor = old_cursor;
        return Err(SyntexError::msg("Invalid atom", self.cursor));
    }

    pub fn parse_character_escape(&mut self, unicode_mode: bool) -> PResult<Character>{
        let old_cursor = self.cursor;

        // character escape
        if self.match_char('f'){
            return Ok(Character::FormFeed)
        }
        if self.match_char('n'){
            return Ok(Character::LineFeed)
        }
        if self.match_char('r'){
            return Ok(Character::Carriage)
        }
        if self.match_char('t'){
            return Ok(Character::HorizontalTab)
        }
        if self.match_char('v'){
            return Ok(Character::VerticalTab)
        }
        
        if self.match_char('c'){
            if self.match_range('A'..='Z'){
                let c = self.lookbehind().unwrap();
                return Ok(Character::ControlChar(c))
            } else{

                self.cursor = old_cursor;

                return Err(SyntexError::msg("expecting A..=Z for character class control.", self.cursor))
            }
        }

        // \0
        if self.match_char('0'){
            if let Some(ahead) = self.peek(){
                // must not be number
                if !('0'..='9').contains(&ahead){
                    self.increment_cusor();
                    return Ok(Character::Char('\0'))
                }
            } else{
                // no input left
                return Ok(Character::Char('\0'))
            }
            self.cursor -= 1;
        }

        // \x00 ~ \xFF
        if self.match_char('x'){
            if let Some(d) = self.parse_two_digits(){
                return Ok(Character::Char(d as char))
            } else{

                self.cursor = old_cursor;

                return Err(SyntexError::msg("invalide escape sequence.", self.cursor))
            }
        }

        // \u
        if let Ok(c) = self.parse_unicode_escape_sequence(unicode_mode){
            return Ok(Character::Char(c))
        }

        // identity escape
        if self.unicode_mode{
            const SPECIAL: &[char] = &[
                '/', '^', '$', '\\', '.', '*', '+', '?', '(', ')', '[', ']', '{', '}', '|'
            ];

            if let Some(ch) = self.peek(){
                if SPECIAL.contains(&ch){
                    self.increment_cusor();
                    return Ok(Character::Char(ch))
                }
            }
        } else{
            // matchs any source charactor but not unicode_id_continue
            if let Some(ch) = self.peek(){
                if !unicode_id_start::is_id_continue(ch){
                    self.increment_cusor();

                    return Ok(Character::Char(ch))
                }
            }
        }

        // reset cursor
        self.cursor = old_cursor;

        return Err(SyntexError::msg("invalid character escape", self.cursor))
    }

    pub fn parse_character_class_escape(&mut self, unicode_mode: bool) -> PResult<CharacterClass>{
        let old_cursor = self.cursor;

        if self.match_char('d'){
            return Ok(CharacterClass::Digits)
        }
        if self.match_char('D'){
            return Ok(CharacterClass::NonDigits)
        }
        if self.match_char('s'){
            return Ok(CharacterClass::WhiteSpace)
        }
        if self.match_char('S'){
            return Ok(CharacterClass::NonWhiteSpace)
        }
        if self.match_char('w'){
            return Ok(CharacterClass::Alphanumeric)
        }
        if self.match_char('W'){
            return Ok(CharacterClass::NonAlphanumeric)
        }

        if unicode_mode{
            if !self.match_char('p'){
                self.cursor = old_cursor;
                return Err(SyntexError::new('p', self.cursor))
            };

        } else{
            if !self.match_char('P'){
                self.cursor = old_cursor;
                return Err(SyntexError::new('P', self.cursor))
            };
        }
        
        if !self.match_char('{'){
            self.cursor = old_cursor;
            return Err(SyntexError::new('{', self.cursor))
        };

        let mut is_name_value = false;

        let mut name = Vec::new();
        loop{
            if self.match_range('a'..='z') || self.match_range('A'..='Z') || self.match_char('_'){
                name.push(self.lookbehind().unwrap());

            } else if self.match_range('0'..='9'){
                is_name_value = true;


            } else{
                break;
            }
        }

        // match value
        let value = if self.match_char('='){
            if is_name_value{
                self.cursor = old_cursor;
                return Err(SyntexError::msg("Invalid unicode property name", self.cursor));
            }

            // try to parse value
            let mut value = Vec::new();

            loop{
                if self.match_range('a'..='z') || self.match_range('A'..='Z') || self.match_range('0'..='9') || self.match_char('_'){
                    value.push(self.lookbehind().unwrap());
    
                } else{
                    break;
                }
            }

            Some(value.into())
        } else{
            None
        };


        if !self.match_char('}'){
            self.cursor = old_cursor;
            return Err(SyntexError::new('}', self.cursor))
        };

        if is_name_value{
            return Ok(CharacterClass::Property(
                UnicodePropertyValue{
                    name: None,
                    value: Some(name.into())
                }
            ))
        } else{
            return Ok(
                CharacterClass::Property(
                    UnicodePropertyValue { 
                        name: Some(name.into()), 
                        value: value
                    }
                )
            )
        }
        
    }

    /// a single class range can either be -a, a- or a-z or just a
    pub fn parse_class_range(&mut self, unicode_mode: bool) -> PResult<ClassRange>{
        let old_cursor = self.cursor;

        let mut special = None;

        // closing class ranges
        if self.peek() == Some(']'){
            self.cursor = old_cursor;
            return Err(SyntexError::new('-', self.cursor))
        }

        'outer:{
            if self.match_char('\\'){

                if self.match_char('b'){
                    special = Some('\u{0008}');
                    break 'outer;
                }

                if unicode_mode{
                    if self.match_char('-'){
                        special = Some('-');
                        break 'outer;
                    }
                }

                if let Ok(c) = self.parse_character_class_escape(unicode_mode){
                    return Ok(ClassRange::CharacterClass(c))
                }

                let c = self.parse_character_escape(unicode_mode)?;
                return Ok(ClassRange::Character(c))
            }
        }
        
        if self.match_char('-'){
            if self.peek() == Some('\\') || self.peek() == Some(']') || self.peek() == Some('-'){

                self.cursor = old_cursor;

                return Err(SyntexError::msg("invalid class range.", self.cursor))
            }
            let atom = self.parse_class_atom(unicode_mode)?;

            return Ok(ClassRange::To(atom))
        };

        let atom = if let Some(special) = special{
            special
        } else{
            self.parse_class_atom(unicode_mode)?
        };

        if self.match_char('-'){
            if self.peek() == Some(']'){
                return Ok(ClassRange::From(atom))
            }

            let b = self.parse_class_atom(unicode_mode)?;

            if atom > b{
                self.cursor = old_cursor;
                return Err(SyntexError::msg("Range out of order in character class.", self.cursor))
            }

            return Ok(ClassRange::Range(atom, b))
        };

        return Ok(ClassRange::Char(atom));
    }

    pub fn parse_class_atom(&mut self, _unicode_mode: bool) -> PResult<char>{

        if let Some(ch) = self.peek(){

            if ch == '\\'{

                self.increment_cusor();

                if self.match_char('-'){
                    return Ok('-')
                }

                if self.match_char('b'){
                    return Ok('\u{0008}')
                }

                self.cursor -= 1;

                return Err(SyntexError::msg("invalide class atom.", self.cursor))
            }

            if ch != '\\' && ch != ']' && ch != '-'{
                self.increment_cusor();
                return Ok(ch)
            }
        };

        return Err(SyntexError::msg("invalid class atom.", self.cursor))
    }

    /// parses the group name
    pub fn parse_group_name(&mut self) -> PResult<Box<[char]>>{
        let mut name = Vec::new();

        let mut is_start = true;

        while self.cursor < self.input.len(){

            if is_start{
                is_start = false;

                if let Some(ch) = self.peek(){
                    if ch == '$' || ch == '_' || unicode_id_start::is_id_start(ch){
                        self.increment_cusor();

                        name.push(ch);
                        continue;
                    }
                }

            } else{

                if let Some(ch) = self.peek(){
                    if ch == '$' || ch == '\u{200D}' || ch == '\u{200C}' || unicode_id_start::is_id_continue(ch){
                        self.increment_cusor();
                        name.push(ch);
                        continue;
                    }
                }
            };

            // not id start or id continue

            if self.match_char('\\'){
                let c = self.parse_unicode_escape_sequence(true)?;
                name.push(c);
                continue;
            };

            // break when no rules matches
            break;
        }

        if name.len() == 0{
            return Err(SyntexError::msg("group identifier cannot be empty.", self.cursor))
        }

        return Ok(name.into());
    }


    pub fn parse_unicode_escape_sequence(&mut self, unicode_mode: bool) -> PResult<char>{

        let old_cursor = self.cursor;

        if !self.match_char('u'){
            self.cursor = old_cursor;
            return Err(SyntexError::new('u', self.cursor))
        };

        // only supports \u0000
        if !unicode_mode{
            
            if let Some(d) = self.parse_four_digits(){
                match char::decode_utf16([d]).next().unwrap(){
                    Ok(c) => {
                        return Ok(c)
                    },
                    Err(_e) => {
                        self.cursor = old_cursor;
                        return Err(SyntexError::msg("unicode escape code must not be a surrogate.", self.cursor))
                    }
                }
            } else{
                self.cursor = old_cursor;
                return Err(SyntexError::msg("expecting four digits for escape sequence.", self.cursor))
            }         
        };

        // \u{ codepoint }
        if self.match_char('{'){
            if let Some(code_point) = self.parse_digits(){

                if !self.match_char('}'){
                    self.cursor = old_cursor;
                    return Err(SyntexError::new('}', self.cursor))
                }
                
                if let Some(c) = char::from_u32(code_point){
                    
                    return Ok(c)
                    
                } else{
                    self.cursor = old_cursor;
                    return Err(SyntexError::msg("invalide unicode code point.", self.cursor - 1));
                }

            }  else{
                self.cursor = old_cursor;
                return Err(SyntexError::msg("expecting code point.", self.cursor));
            }
        }

        // \u xxxx \u xxxx
        if let Some(head) = self.parse_four_digits(){

            // non surrogate, utf16 character
            if !(0xD800..=0xDFFF).contains(&head){
                for c in char::decode_utf16([head]){
                    if let Ok(p) = c{
                        return Ok(p)
                    }
                };
            }

            // not a lead surrogate
            if !(0xD800..=0xDBFF).contains(&head){
                self.cursor = old_cursor;
                return Err(SyntexError::msg("invalid unicode lead surrogate.", self.cursor))
            }

            if !self.match_char('\\'){
                self.cursor = old_cursor;
                return Err(SyntexError::new('\\', self.cursor));
            }

            if !self.match_char('u'){
                self.cursor = old_cursor;
                return Err(SyntexError::new('u', self.cursor))
            }

            if let Some(trail) = self.parse_four_digits(){
                if !(0xDC00..=0xDFFF).contains(&trail){
                    self.cursor = old_cursor;
                    return Err(SyntexError::msg("invalide unicode trail surrogate", self.cursor))
                }

                for c in char::decode_utf16([head, trail]){
                    if let Ok(ch) = c{
                        return Ok(ch)
                    }
                };
            } 
        }

        self.cursor = old_cursor;
        return Err(SyntexError::msg("expecting 4 digits surrogate pairs", self.cursor))
    }

    pub fn parse_four_digits(&mut self) -> Option<u16>{

        let start = self.cursor;

        for _ in 0..4{
            if self.match_range('A'..='F') || self.match_range('a'..='f') || self.match_range('0'..='9'){

            } else{
                self.cursor = start;
                return None
            }
        };

        let c = &self.input[start..start+4];

        let mut i = 0;

        i += c[0].to_digit(16).unwrap() as u16 * 16 * 16 * 16;
        i += c[1].to_digit(16).unwrap() as u16 * 16 * 16;
        i += c[2].to_digit(16).unwrap() as u16 * 16;
        i += c[3].to_digit(16).unwrap() as u16;

        return Some(i)
    }

    pub fn parse_two_digits(&mut self) -> Option<u8>{
        let start = self.cursor;

        for _ in 0..2{
            if self.match_range('A'..='F') || self.match_range('a'..='f') || self.match_range('0'..='9'){

            } else{
                self.cursor = start;
                return None
            }
        };

        let c = &self.input[start..start+2];

        let mut i = 0;

        i += c[0].to_digit(16).unwrap() as u8 * 16;
        i += c[1].to_digit(16).unwrap() as u8;

        return Some(i)
    }

    pub fn parse_digits(&mut self) -> Option<u32>{

        let start = self.cursor;

        loop{
            if self.match_range('A'..='F') || self.match_range('a'..='f') || self.match_range('0'..='9'){

            } else{
                break;
            }
        };

        if start == self.cursor{
            return None
        }

        let mut digits = (self.cursor - start) as u32;
        let slice = &self.input[start..self.cursor]; 

        let mut i = 0;

        for c in slice{
            digits -= 1;
            i += c.to_digit(16).unwrap() * 16u32.pow(digits);
        };

        return Some(i)
    }

    pub fn parse_integer(&mut self) -> Option<u32> {
        let start = self.cursor;
        let mut end = self.cursor;

        while self.cursor <= self.input.len() {
            if self.match_range('0'..'9') {
                end += 1;
            } else {
                break;
            }
        }

        if start == end {
            return None;
        }

        let slice = &self.input[start..end];

        let mut i = 0;

        let mut digits = slice.len() as u32;

        for ch in slice {
            digits -= 1;
            i += ch.to_digit(10).unwrap() * 10u32.pow(digits);
        }

        return Some(i);
    }
}