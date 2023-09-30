use std::collections::{HashSet, HashMap};



pub struct IRRegex{

}

impl IRRegex{
    pub fn new(pattern:&str, flags:&str) -> Result<Self, String>{
        let mut i = false;
        let mut m = false;
        let mut s = false;
        let mut u = false;
        for c in flags.chars(){
            match c{
                'd' => {},
                'g' => {},
                'i' => i=true,
                'm' => m=true,
                's' => s=true,
                'u' => u=true,
                'y' => {},
                _ => return Err(format!("unknown regex flag: {}", c))
            }
        };

        let pat = regex_parser::pattern(pattern, u, true);

        let pat = match pat{
            Ok(p) => p,
            Err(e) => {
                return Err(e.to_string())
            }
        };

        let mut rer = RegexRecord{
            ignore_case: i,
            multiline: m,
            dot_all: s,
            unicode: u,
            capture_group_count: 0,
            group_names: Default::default(),
            ir: Default::default()
        };

        pat.compile(&mut rer)?;

        todo!()
    }
}

struct Pattern{
    disjunction: Disjunction
}

impl Pattern{
    fn count_left_capturing_parens_within(&self) -> usize{
        self.disjunction.count_left_capturing_parens_within()
    }
}

struct Disjunction{
    alternatives: Vec<Alternative>
}

impl Disjunction{
    fn count_left_capturing_parens_within(&self) -> usize{
        let mut i = 0;
        for a in &self.alternatives{
            i += a.count_left_capturing_parens_within()
        }
        return i
    }
}

struct Alternative{
    terms: Vec<Term>
}

impl Alternative{
    fn count_left_capturing_parens_within(&self) -> usize{
        let mut i = 0;
        for t in &self.terms{
            i += t.count_left_capturing_parens_within();
        }
        return i;
    }
}

enum Term{
    Assertion(Assertion),
    Atom(Atom),
    AtomQuantifier(Atom, Quantifier)
}

impl Term{
    fn count_left_capturing_parens_within(&self) -> usize{
        match self{
            Self::Assertion(a) => a.count_left_capturing_parens_within(),
            Self::Atom(a) => a.count_left_capturing_parens_within(),
            Self::AtomQuantifier(a, _q) => a.count_left_capturing_parens_within(),
        }
    }
}

enum Assertion{
    /// ^
    Circumflex,
    /// $
    Dollar,
    /// \b
    Slashb,
    /// \B
    SlashB,
    /// (?= Disjunction)
    Eq(Disjunction),
    /// (?! Disjunction)
    Not(Disjunction),
    /// (?<= Disjunction)
    Lteq(Disjunction),
    /// (?<! Disjunction)
    LtNot(Disjunction)
}

impl Assertion{
    fn count_left_capturing_parens_within(&self) -> usize{
        match self{
            Self::Eq(e) => e.count_left_capturing_parens_within(),
            Self::Not(n) => n.count_left_capturing_parens_within(),
            Self::LtNot(l) => l.count_left_capturing_parens_within(),
            Self::Lteq(l) => l.count_left_capturing_parens_within(),
            _ => 0
        }
    }
}

/// QuantifierPrefix ?
struct Quantifier{
    /// ?
    optional: bool,
    prefix: QuantifierPrefix
}

enum QuantifierPrefix{
    /// *
    Star,
    /// +
    Plus,
    /// ?
    Optional,
    /// { DecimlDigit }
    Digit(DecimalDigits),
    /// { DecimalDigit ,}
    DigitComma(DecimalDigits),
    /// { DecimalDigit, DecimalDigit }
    MultiDigit(DecimalDigits, DecimalDigits),
}

enum Atom{
    PatternCharacter(char),
    /// .
    Dot,
    /// \ AtomEscape
    AtomEscape(AtomEscape),
    CharacterClass(CharacterClass),
    /// ( opt GroupSpecifier Disjunction )
    GroupDisjunction{
        specifier: Option<String>,
        disjunction: Disjunction
    },
    /// (?: Disjunction)
    Disjunction(Disjunction),
}

impl Atom{
    fn count_left_capturing_parens_within(&self) -> usize{
        match self{
            Self::GroupDisjunction { specifier:_, disjunction } => {
                let i = disjunction.count_left_capturing_parens_within();
                return i + 1;
            },
            Self::Disjunction(d) => d.count_left_capturing_parens_within(),
            _ => 0
        }
    }
}

enum AtomEscape{
    DecimalEscape(usize),
    CharacterClassEscape(CharacterClassEscape),
    CharacterEscape(CharacterEscape),
    /// k GroupName
    GroupName(String)
}

enum CharacterEscape{
    /// f n r t v
    ControllEscape(char),
    /// c AsciiLetter
    AsciiLetter(char),
    /// 0 [lookahead ∉ DecimalDigit]
    Zero,
    HexEscapeSequence(u8),
    RegExpUnicodeEscapeSequence(RegExpUnicodeEscapeSequence),
    IdentityEscape(IdentityEscape),
}

impl CharacterEscape{
    fn character_value(&self) -> u32{
        match self{
            Self::ControllEscape(c) => {
                match *c{
                    'f' => '\u{0009}' as u32,
                    'n' => '\u{000A}' as u32,
                    'r' => '\u{000B}' as u32,
                    't' => '\u{000C}' as u32,
                    'v' => '\u{000D}' as u32,
                    _ => unreachable!()
                }
            },
            Self::AsciiLetter(a) => {
                let i = *a as u32;
                i % 32
            }
            Self::Zero => '\u{0000}' as u32,
            Self::HexEscapeSequence(h) => {
                *h as u32
            }
            Self::IdentityEscape(i) => i.character_value(),
            Self::RegExpUnicodeEscapeSequence(r) => r.character_value(),
        }
    }
}


/// ? GroupName
struct GroupSpecifier(GroupName);

///  < RegExpIdentifierName > 
struct GroupName(RegExpIdentifierName);

struct RegExpIdentifierName(Vec<u32>);

impl RegExpIdentifierName{
    fn code_points(&self) -> &[u32]{
        &self.0
    }
}

enum RegExpIdentifierStart{
    StartChar(char),
    /// \ RegExpUnicodeEscapeSequence
    RegExpUnicodeEscapeSequence(RegExpUnicodeEscapeSequence),
    Surrogate(char)
}

impl RegExpIdentifierStart{
    fn codepoint(&self) -> u32{
        match self{
            Self::StartChar(c) => *c as u32,
            Self::RegExpUnicodeEscapeSequence(r) => r.character_value(),
            Self::Surrogate(c) => *c as u32
        }
    }
}

enum RegExpIdentifierPart{
    PartChar(char),
    /// \ RegExpUnicodeEscapeSequence
    RegExpUnicodeEscapeSequence(RegExpUnicodeEscapeSequence),
    Surrogate(char)
}

impl RegExpIdentifierPart{
    fn codepoint(&self) -> u32{
        match self{
            Self::PartChar(c) => *c as u32,
            Self::RegExpUnicodeEscapeSequence(r) => r.character_value(),
            Self::Surrogate(c) => *c as u32
        }
    }
}

enum RegExpUnicodeEscapeSequence{
    LeadTrail(u16, u16),
    Lead(u16),
    Trail(u16),
    Non(u16),
    Hex(u16),
    CodePoint(u32)
}

impl RegExpUnicodeEscapeSequence{
    fn character_value(&self) -> u32{
        match self {
            Self::LeadTrail(l, t) => {
                surrogate_to_char(*l, *t) as u32
            },
            Self::Hex(h) => *h as u32,
            Self::Lead(l) => *l as u32,
            Self::Trail(t) => *t as u32,
            Self::Non(n) => *n as u32,
            Self::CodePoint(c) => *c as u32
        }
    }
}

enum IdentityEscape{
    SyntaxCharacter(char),
    /// `/`
    Slash,
    SourceCharacter(char)
}

impl IdentityEscape{
    fn character_value(&self) -> u32{
        match self{
            Self::Slash => '/' as u32,
            Self::SyntaxCharacter(c) => *c as u32,
            Self::SourceCharacter(c) => *c as u32
        }
    }
}

enum CharacterClassEscape{
    d,
    D,
    s,
    S,
    w,
    W,
    p(UnicodePropertyValueExpression),
    P(UnicodePropertyValueExpression)
}

impl CharacterClassEscape{

}

enum UnicodePropertyValueExpression{
    NameValue{
        name:String,
        value: String,
    },
    NameOrValue(String)
}

struct DecimalEscape{
    start: char,
    digit: Vec<char>
}

struct CharacterClass{
    /// ^
    has_circumflex: bool,
    class_ranges: ClassRanges
}

enum ClassRanges{
    Empty,
    Ranges(NonemptyClassRanges)
}

enum NonemptyClassRanges{
    Atom(ClassAtom),
    AtomNoDash(ClassAtom, Box<NonemptyClassRangesNoDash>),
    AtomRanges(ClassAtom, ClassAtom, Box<ClassRanges>)
}

enum NonemptyClassRangesNoDash{
    Atom(ClassAtom),
    AtomNoDash(ClassAtom, Box<NonemptyClassRangesNoDash>),
    AtomRanges(ClassAtom, ClassAtom, Box<ClassRanges>)
}

enum ClassAtom{
    Dash,
    SourceCharacter(char),
    ClassEscape(ClassEscape)
}

impl ClassAtom{
    fn is_character_class(&self) -> bool{
        match self{
            Self::Dash => false,
            Self::SourceCharacter(_) => false,
            Self::ClassEscape(c) => c.is_character_class()
        }
    }

    fn character_value(&self) -> Option<u32>{
        match self{
            Self::Dash => Some('\u{002D}' as u32),
            Self::SourceCharacter(c) => Some(*c as u32),
            Self::ClassEscape(c) => c.character_value()
        }
    }
}
enum ClassEscape{
    b,
    Dash,
    CharacterClassEscape(CharacterClassEscape),
    CharacterEscape(CharacterEscape)
}

impl ClassEscape{
    fn is_character_class(&self) -> bool{
        match self{
            Self::CharacterClassEscape(_) => true,
            _ => false
        }
    }

    fn character_value(&self) -> Option<u32>{
        match self{
            Self::b => Some('\u{0008}' as u32),
            Self::Dash => Some('\u{002D}' as u32),
            Self::CharacterEscape(c) => Some(c.character_value()),
            Self::CharacterClassEscape(_) => None,
        }
    }
}

type DecimalDigits = Vec<char>;

pub fn surrogate_to_char(lead:u16, trail:u16) -> char{
    char::decode_utf16([lead, trail]).next().unwrap().expect("unpaired surrogate: ")
}


peg::parser!{
    grammar regex_parser() for str{
        pub(super) rule pattern(unicode_mode:bool, N:bool) -> Pattern
        = d:disjunction(unicode_mode, N) {?
            if d.count_left_capturing_parens_within() >= 2^32 -1{
                Err("capturing parenthesis must not exceed 2^32 -1")
            } else{
                Ok(Pattern{
                    disjunction:d
                })
            }
        }

        rule disjunction(unicode_mode:bool, N:bool) -> Disjunction
        = a:(alternative(unicode_mode, N) ** "|") {Disjunction { alternatives: a }}

        rule alternative(unicode_mode:bool, N:bool) -> Alternative
        = t:(term(unicode_mode, N)*) {Alternative { terms: t }}

        rule term(unicode_mode:bool, N:bool) -> Term
        = a:assertion(unicode_mode, N) {Term::Assertion(a)}
        / a:atom(unicode_mode, N) {Term::Atom(a)}
        / a:atom(unicode_mode, N) q:quantifier() {Term::AtomQuantifier(a, q)}
        
        rule assertion(unicode_mode:bool, N:bool) -> Assertion
        = "^" {Assertion::Circumflex}
        / "$" {Assertion::Dollar}
        / "\\b" {Assertion::Slashb}
        / "\\B" {Assertion::SlashB}
        / "(" "?" "=" d:disjunction(unicode_mode, N) ")" {Assertion::Eq(d)}
        / "(" "?" "!" d:disjunction(unicode_mode, N) ")" {Assertion::Not(d)}
        / "(" "?" "<" "=" d:disjunction(unicode_mode, N) ")" {Assertion::Lteq(d)}
        / "(" "?" "<" "!" d:disjunction(unicode_mode, N) ")" {Assertion::LtNot(d)}

        rule quantifier() -> Quantifier
        = q:quantifier_prefix() {Quantifier { optional: false, prefix: q }}
        / q:quantifier_prefix() "?" {Quantifier { optional: true, prefix: q }}

        rule quantifier_prefix() -> QuantifierPrefix
        = "*" {QuantifierPrefix::Star}
        / "+" {QuantifierPrefix::Plus}
        / "?" {QuantifierPrefix::Optional}
        / "{" d:decimal_digits() "}" {QuantifierPrefix::Digit(d)}
        / "{" d:decimal_digits() "," "}" {QuantifierPrefix::DigitComma(d)}
        / "{" d:decimal_digits() "," d2:decimal_digits() "}" {?
            if d > d2{
                Err("first quantifier must be smaller then the second quantifier")
            } else{
                Ok(QuantifierPrefix::MultiDigit(d, d2))
            }
        }

        rule atom(unicode_mode:bool, N:bool) -> Atom
        = p:pattern_character() {Atom::PatternCharacter(p)}
        / "." {Atom::Dot}
        / "\\" a:atom_escape(unicode_mode, N) {Atom::AtomEscape(a)}
        / c:character_class(unicode_mode) {Atom::CharacterClass(c)}
        / "(" g:$((group_specifier(unicode_mode)))? d:disjunction(unicode_mode, N) ")" {Atom::GroupDisjunction { specifier: g.map(|s|s.to_string()), disjunction: d }}
        / "(" "?" ":" d:disjunction(unicode_mode, N) ")" {Atom::Disjunction(d)}


        rule syntax_character() -> char
        = c:(['^' | '$' | '\\' | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '|']) {c}

        rule pattern_character() -> char
        = !syntax_character() c:(['\u{0000}'..='\u{FFFF}']) {c}

        rule atom_escape(unicode_mode:bool, N:bool) -> AtomEscape
        = d:$(decimal_escape(unicode_mode)) {?
            match d.parse::<usize>(){
                Ok(v) => Ok(AtomEscape::DecimalEscape(v)),
                Err(_) => Err("decimal escape out of range")
            }
        }
        / c:character_class_escape(unicode_mode) {AtomEscape::CharacterClassEscape(c)}
        / c:character_escape(unicode_mode) {AtomEscape::CharacterEscape(c)}
        / "k" g:$(group_name(unicode_mode)) {? 
            if N{
                Ok(AtomEscape::GroupName(g.to_string()))
            } else{
                Err("'k group_name' only avaliable when N is true")
            }
        }

        rule character_escape(unicode_mode:bool) -> CharacterEscape
        = c:(['f' | 'n' | 'r' | 't' | 'v']) {CharacterEscape::ControllEscape(c)}
        / "c" a:(['a'..='z' | 'A'..='Z']) {CharacterEscape::AsciiLetter(a)}
        / "0" !decimal_digit() {CharacterEscape::Zero}
        / h:$(hex_escape_sequence()) {CharacterEscape::HexEscapeSequence(u8::from_str_radix(h, 16).unwrap())}
        / r:regex_unicode_escape_sequence(unicode_mode) {CharacterEscape::RegExpUnicodeEscapeSequence(r)}
        / i:identity_escape(unicode_mode) {CharacterEscape::IdentityEscape(i)}

        rule group_specifier(unicode_mode:bool) -> GroupSpecifier
        = "?" g:group_name(unicode_mode) {GroupSpecifier(g)}

        rule group_name(unicode_mode:bool) -> GroupName
        = "<" r:regex_identifier_name(unicode_mode) ">" {GroupName(r)}

        rule regex_identifier_name(unicode_mode:bool) -> RegExpIdentifierName
        = start:regex_identifier_start(unicode_mode) parts:(regex_identifier_part(unicode_mode)*) {
            let mut i = vec![start];
            i.extend_from_slice(&parts);
            RegExpIdentifierName(i)
        }

        rule regex_identifier_start(unicode_mode:bool) -> u32
        = c:[_] {? 
            if c=='$' || c=='_' || unicode_id_start::is_id_start(c) {
                Ok(c as u32)
            } else{
                Err("character must be id_start or $ or _")
            }
        }
        / "\\" r:regex_unicode_escape_sequence(true) {
            r.character_value()
        } 
        // lead:unicode_lead_surrogate() trail:unicode_trail_surrogate() {? if unicode_mode{Err("unicode surrogate only supported in non unicode mode")} else{ Ok(surrogate_to_char(lead, trail))}}
        / c:[_] {? 
            if unicode_mode{
                Err("")
            } else {
                Ok(c as u32)
            }
        }

        rule regex_identifier_part(unicode_mode:bool) -> u32
        = c:[_] {? 
            if c=='$' || c=='\u{200C}' || c=='\u{200D}' || unicode_id_start::is_id_continue(c) {
                Ok(c as u32)
            } else{
                Err("character must be id_continue")
            }
        }
        / "\\" r:regex_unicode_escape_sequence(true) {
            r.character_value()
        }
        //lead:unicode_lead_surrogate() trail:unicode_trail_surrogate() {? if unicode_mode{Err("unicode surrogate only supported in non unicode mode")} else{ Ok(surrogate_to_char(lead, trail))}}
        / c:[_] {? 
            if unicode_mode{
                Err("")
            } else{
                Ok(c as u32)
            }
        }

        rule regex_unicode_escape_sequence(unicode_mode:bool) -> RegExpUnicodeEscapeSequence
        = "u" lead:hex_lead_surrogate() "\\u" trail:hex_trail_surrogate() {? if unicode_mode {Ok(RegExpUnicodeEscapeSequence::LeadTrail(lead, trail))} else{ Err("") }}
        / "u" lead:hex_lead_surrogate() {? if unicode_mode {Ok(RegExpUnicodeEscapeSequence::Lead(lead))} else {Err("")}}
        / "u" trail:hex_trail_surrogate() {? if unicode_mode {Ok(RegExpUnicodeEscapeSequence::Trail(trail))} else {Err("")}}
        / "u" n:hex_non_surrogate() {? if unicode_mode {Ok(RegExpUnicodeEscapeSequence::Non(n))} else{Err("")}}
        / "u" h:$(hex4digit()) {?
            if !unicode_mode{
                let u = u16::from_str_radix(h, 16).unwrap();
                Ok(RegExpUnicodeEscapeSequence::Hex(u))

            } else{
                Err("")
            }
        }
        / "\\u" "{" c:$(hex_digit()*) "}" {?
            let c = u32::from_str_radix(c, 16).unwrap(); 

            if c > 0x10FFFF{
                Err("codepoint must be <= 0x10FFFF")
            } else{
                Ok(RegExpUnicodeEscapeSequence::CodePoint(c))
            }
        }

        rule hex_lead_surrogate() -> u16
        = h:hex4digit() {? let s = u16::from_str_radix(h, 16).expect("parse error"); if (0xD800..=0xDBFF).contains(&s){Ok(s)} else {Err("unicode lead surrogate must be in range 0xD800 0xDBFF")}}

        rule hex_trail_surrogate() -> u16
        = h:hex4digit() {? let s = u16::from_str_radix(h, 16).expect("parse error"); if (0xDC00..=0xDFFF).contains(&s){Ok(s)} else {Err("unicode trail surrogate must be in range 0xDC00 0xDFFF")}}

        rule hex_non_surrogate() -> u16
        = h:hex4digit() {? let s = u16::from_str_radix(h, 16).expect("parse error"); if (0xD800..=0xDFFF).contains(&s){Ok(s)} else {Err("unicode non surrogate must be in range 0xD800 0xDFFF")}}

        rule identity_escape(unicode_mode:bool) -> IdentityEscape
        = s:syntax_character() {? 
            if unicode_mode{
                Ok(IdentityEscape::SyntaxCharacter(s))
            } else{
                Err("")
            }
        }
        / "/" {?
            if unicode_mode{
                Ok(IdentityEscape::Slash)
            } else{
                Err("")
            }
        }
        / c:[_] {?
            if !unicode_mode && !unicode_id_start::is_id_continue(c){
                Ok(IdentityEscape::SourceCharacter(c))
            } else{
                Err("")
            }
        }

        rule decimal_escape(unicode_mode:bool) -> DecimalEscape
        = start:['1'..='9'] d:decimal_digits()? !decimal_digit() {DecimalEscape { start: start, digit: d.unwrap_or(Vec::new()) }}


        rule character_class_escape(unicode_mode:bool) -> CharacterClassEscape
        = "d" {CharacterClassEscape::d}
        / "D" {CharacterClassEscape::D}
        / "s" {CharacterClassEscape::s}
        / "S" {CharacterClassEscape::S}
        / "w" {CharacterClassEscape::w}
        / "W" {CharacterClassEscape::W}
        / "p{" u:unicode_property_value_expression() "}" {CharacterClassEscape::p(u)}
        / "P{" u:unicode_property_value_expression() "}" {CharacterClassEscape::P(u)}

        rule unicode_property_value_expression() -> UnicodePropertyValueExpression
        = name:$(unicode_property_name()) "=" v:$(unicode_property_value()) {?
            let names = &[
                "General_Category",
                "gc",
                "Script",
                "sc",
                "Script_Extensions",
                "scx"
            ];

            if !names.contains(&name){
                Err("unknown unicode property name.")
            } else{
                Ok(UnicodePropertyValueExpression::NameValue { name: name.to_string(), value: v.to_string() })
            }
            
        }
        / l:$(unicode_property_value()) {UnicodePropertyValueExpression::NameOrValue(l.to_string())}

        rule unicode_property_name() -> Vec<char>
        = a:['a'..='z' | 'A'..='Z' | '_']* {a}

        rule unicode_property_value() -> Vec<char>
        = d:['a'..='z' | 'A'..='Z' | '_' | '0'..='9']* {d}

        rule character_class(unicode_mode:bool) -> CharacterClass
        = "[" !"^" r:class_ranges(unicode_mode) "]" {CharacterClass { has_circumflex: false, class_ranges: r }}
        / "[^" r:class_ranges(unicode_mode) "]" {CharacterClass { has_circumflex: true, class_ranges: r }}

        rule class_ranges(unicode_mode:bool) -> ClassRanges
        = n:non_empty_class_ranges(unicode_mode) {ClassRanges::Ranges(n)}
        / {ClassRanges::Empty}

        rule non_empty_class_ranges(unicode_mode:bool) -> NonemptyClassRanges
        = c:class_atom(unicode_mode) {NonemptyClassRanges::Atom(c)}
        / c:class_atom(unicode_mode) n:non_empty_class_ranges_nodash(unicode_mode) { NonemptyClassRanges::AtomNoDash(c, Box::new(n))}
        / c:class_atom(unicode_mode) "-" c1:class_atom(unicode_mode) r:class_ranges(unicode_mode) {?
            // early error
            if c.is_character_class() && c1.is_character_class(){
                Err("both atoms are character class")
            } else if !c.is_character_class() && !c1.is_character_class() {
                if c.character_value().unwrap_or(0) > c1.character_value().unwrap_or(0){
                    Err("character value of first atom must be smaller then the second atom")
                } else{
                    Ok(NonemptyClassRanges::AtomRanges(c, c1, Box::new(r)))
                }
            }else{
                Ok(NonemptyClassRanges::AtomRanges(c, c1, Box::new(r)))
            }
        }

        rule non_empty_class_ranges_nodash(unicode_mode:bool) -> NonemptyClassRangesNoDash
        = c:class_atom(unicode_mode) {NonemptyClassRangesNoDash::Atom(c)}
        / c:class_atom_nodash(unicode_mode) n:non_empty_class_ranges_nodash(unicode_mode) {NonemptyClassRangesNoDash::AtomNoDash(c, Box::new(n))}
        / c:class_atom_nodash(unicode_mode) "-" c1:class_atom(unicode_mode) r:class_ranges(unicode_mode) {?

            // early error
            if c.is_character_class() && c1.is_character_class(){
                Err("both atoms are character class")
            } else if !c.is_character_class() && !c1.is_character_class(){
                if c.character_value().unwrap_or(0) > c1.character_value().unwrap_or(0){
                    Err("character value of the first atom must be smaller then the second atom.")
                } else{
                    Ok(NonemptyClassRangesNoDash::AtomRanges(c, c1, Box::new(r)))
                }
            } else{
                Ok(NonemptyClassRangesNoDash::AtomRanges(c, c1, Box::new(r)))
            }
            
        }

        rule class_atom(unicode_mode:bool) -> ClassAtom
        = "-" {ClassAtom::Dash}
        / c:class_atom_nodash(unicode_mode) {c}

        rule class_atom_nodash(unicode_mode:bool) -> ClassAtom
        = s:[^ '\\' | ']' | '-'] {ClassAtom::SourceCharacter(s)}
        / "\\" c:class_escape(unicode_mode) {ClassAtom::ClassEscape(c)}

        rule class_escape(unicode_mode:bool) -> ClassEscape
        = "b" {ClassEscape::b}
        / "-" {? if unicode_mode{Ok(ClassEscape::Dash)} else {Err("")}}
        / c:character_class_escape(unicode_mode) {ClassEscape::CharacterClassEscape(c)}
        / c:character_escape(unicode_mode) {ClassEscape::CharacterEscape(c)}

        rule hex_escape_sequence() -> [char;2]
        = "x" a:hex_digit() b:hex_digit() {[a, b]}

        rule hex4digit() -> &'input str
        = h:$(['0'..='9' | 'a'..= 'f' | 'A'..='F']['0'..='9' | 'a'..= 'f' | 'A'..='F']['0'..='9' | 'a'..= 'f' | 'A'..='F']['0'..='9' | 'a'..= 'f' | 'A'..='F']) {h}

        rule hex_digit() -> char
        = h:(['0'..='9' | 'a'..='f' | 'A'..='F']) {h}

        rule decimal_digits() -> DecimalDigits
        = d:decimal_digit()* {d}

        rule decimal_digit() -> char
        = d:['0'..='9'] {d}

        rule UnicodeLeadSurrogate() -> u16
        = h:$("0xD" (['0'..='9'] / ['A'..='F'] / ['a'..='f'])(['0'..='9'] / ['A'..='F'] / ['a'..='f'])(['0'..='9'] / ['A'..='F'] / ['a'..='f'])(['0'..='9'] / ['A'..='F'] / ['a'..='f'])) {
            h.parse::<u16>().unwrap()
        }
    }
}

/// referenced from libregexp in quickjs
#[derive(serde::Serialize)]
pub enum RegexIR{
    /// never used
    Invalid,
    Char(u16),
    Char32(u32),
    Dot,
    /// same as dot but match any character including line terminator
    Any,
    LineStart,
    LindEnd,
    Goto(u32),
    SplitGotoFirst(u32),
    SplitNextFirst(u32),
    Match,
    /// save start position
    SaveStart(u32),
    /// save end position, must come after saved_start
    SaveEnd(u32),
    /// decrement the top the stack and goto if != 0
    Loop(u32),
    /// push integer on the stack
    PushU32(u32),
    Drop,
    WordBoundary,
    NotWordBoundary,
    BackReference(u32),
    /// must come after back_reference
    BackwardBackReference(u32),
    /// variable length
    Range(Box<[std::ops::Range<u32>]>),
    Lookahead(u32),
    NegativeLookahead(u32),
    /// push the character position on the stack
    PushCharPos,
    /// pop one stack element and jump if equal to the character
    BNECharPos(u32),
    /// go to the previous char
    Prev,
    SimpleGreedyQuant()
}

struct RegexRecord{
    ignore_case:bool,
    multiline:bool,
    dot_all: bool,
    unicode: bool,
    capture_group_count: usize,
    group_names: HashMap<String, usize>,
    ir: Vec<RegexIR>
}


impl Pattern{
    fn compile(&self, rer:&mut RegexRecord) -> Result<(), String>{
        self.disjunction.compile(rer, false)
    }
}

impl Disjunction{
    fn compile(&self, rer:&mut RegexRecord, is_backward:bool) -> Result<(), String>{

        Ok(())
    }
}

impl Alternative{
    fn compile(&self, rer:&mut RegexRecord, is_backward:bool) -> Result<(), String>{

        if self.terms.len() == 0{
            // will always return match
            
        } else{
            for t in &self.terms{
                t.compile(rer, is_backward)?;
            }
        }

        return Ok(())
    }
}

impl Term{
    fn compile(&self, rer:&mut RegexRecord, is_backward:bool) -> Result<(), String>{
        match self{
            Term::Assertion(a) =>  a.compile(rer)?,
            Term::Atom(a) => a.compile(rer, is_backward)?,
            Term::AtomQuantifier(a, q) => {
                let atom_start = rer.ir.len();

                // compile atom
                a.compile(rer, is_backward)?;
            }
        }
        Ok(())
    }
}

impl Assertion{
    fn compile(&self, rer:&mut RegexRecord) -> Result<(), String>{
        let mut is_neg;
        let mut is_backward;
        let mut disjunc;

        match self{
            Self::Circumflex => {
                rer.ir.push(RegexIR::LineStart);
                return Ok(())
            },
            Self::Dollar => {
                rer.ir.push(RegexIR::LindEnd);
                return Ok(())
            }
            Self::Slashb => {
                rer.ir.push(RegexIR::WordBoundary);
                return Ok(())
            }
            Self::SlashB => {
                rer.ir.push(RegexIR::NotWordBoundary);
                return Ok(())
            }
            Self::Not(d) => {
                is_neg = true;
                is_backward = false;
                disjunc = d;
            }
            Self::Eq(d) => {
                is_neg = false;
                is_backward = false;
                disjunc = d;
            }
            Self::LtNot(d) => {
                is_neg = true;
                is_backward = true;
                disjunc = d;
            }
            Self::Lteq(d) => {
                is_neg = false;
                is_backward = true;
                disjunc = d;
            }
        };

        let pos = rer.ir.len();
        rer.ir.push(RegexIR::Lookahead(0));

        disjunc.compile(rer, is_backward)?;

        rer.ir.push(RegexIR::Match);

        if is_neg{
            rer.ir[pos] = RegexIR::NegativeLookahead((rer.ir.len() - (pos + 1)) as u32);
        } else{
            rer.ir[pos] = RegexIR::Lookahead((rer.ir.len() - (pos +1)) as u32);
        }
        
        Ok(())
    }
}

impl Atom{
    fn compile(&self, rer:&mut RegexRecord, is_backward:bool) -> Result<(), String>{
        match self{
            Self::PatternCharacter(c) => {
                if is_backward{
                    rer.ir.push(RegexIR::Prev);
                }

                if rer.ignore_case{
                    // canonicalise
                }

                rer.ir.push(RegexIR::Char32(*c as u32));

                if is_backward{
                    rer.ir.push(RegexIR::Prev)
                }
            }
            Self::Dot => {
                if is_backward{
                    rer.ir.push(RegexIR::Prev);
                }

                rer.ir.push(RegexIR::Dot);

                if is_backward{
                    rer.ir.push(RegexIR::Prev)
                }
            }
            Self::Disjunction(d) => {
                d.compile(rer, is_backward)?;
            }
            Self::GroupDisjunction { specifier, disjunction } => {
                let capture_index = rer.capture_group_count;
                rer.capture_group_count += 1;

                // insert groupname
                if let Some(name) = &specifier{
                    
                    if rer.group_names.contains_key(name){
                        return Err(format!("Duplicated group name: '{}'", name))
                    }

                    rer.group_names.insert(name.clone(), capture_index);
                    
                }

                if is_backward{
                    rer.ir.push(RegexIR::SaveEnd(capture_index as u32))
                } else{
                    rer.ir.push(RegexIR::SaveStart(capture_index as u32))
                }

                disjunction.compile(rer, is_backward)?;

                if is_backward{
                    rer.ir.push(RegexIR::SaveStart(capture_index as u32))
                } else{
                    rer.ir.push(RegexIR::SaveEnd(capture_index as u32));
                }
            }

            Self::AtomEscape(a) => {
                a.compile(rer, is_backward)?;
            }
            Self::CharacterClass(c) => {
                c.compile(rer, is_backward)?;
            }
        }
        

        Ok(())
    }
}

impl AtomEscape{
    fn compile(&self, rer:&mut RegexRecord, is_backward:bool) -> Result<(), String>{
        match self{
            Self::DecimalEscape(d) => {
                if *d >= rer.capture_group_count{
                    return Err(format!("Cannot use capture group index '{}', used before defined.", *d))
                }

                if is_backward{
                    rer.ir.push(RegexIR::BackwardBackReference(*d as u32));
                } else{
                    rer.ir.push(RegexIR::BackReference(*d as u32));
                }
            }
            Self::GroupName(name) => {
                // search for group index
                let re = rer.group_names.get(name);
                let capture_group_index = match re{
                    Some(v) => *v as u32,
                    None => return Err(format!("Cannot find capture group {}", name))
                };

                if is_backward{
                    rer.ir.push(RegexIR::BackwardBackReference(capture_group_index))
                } else{
                    rer.ir.push(RegexIR::BackReference(capture_group_index))
                }
            }
            Self::CharacterClassEscape(c) => {
                c.compile(rer, is_backward)?;
            }
            Self::CharacterEscape(c) => {
                
            }
        }
        
        return Ok(())
    }
}

impl CharacterClassEscape{
    fn compile(&self, rer:&mut RegexRecord, is_backward:bool) -> Result<(), String>{
        if is_backward{
            rer.ir.push(RegexIR::Prev);
        }
        match self{
            Self::d => {
                rer.ir.push(RegexIR::Range(Box::new([0x0030..0x0039+1])));
            }
            Self::D => {
                rer.ir.push(RegexIR::Range(Box::new([0x0000..0x0030, 0x0039..u32::MAX])));
            }
            Self::s => {
                rer.ir.push(RegexIR::Range(Box::new([])));
            }
            _ => {}
        }

        if is_backward{
            rer.ir.push(RegexIR::Prev);
        }

        return Ok(())
    }
}

impl CharacterClass{
    fn compile(&self, rer:&mut RegexRecord, is_backward:bool) -> Result<(), String>{
        return Ok(())
    }
}