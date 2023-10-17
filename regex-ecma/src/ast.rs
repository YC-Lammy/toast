use alloc::string::String;

#[derive(Debug, Clone, PartialEq)]
pub struct CaptureGroup{
    pub name: Option<String>,
    pub is_capturing: bool,
}

#[derive(Debug, PartialEq)]
pub struct Pattern {
    pub disjunction: Disjunction,
    pub capture_groups: Vec<CaptureGroup>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Disjunction {
    pub alternatives: Vec<Alternative>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Alternative {
    pub terms: Vec<Term>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Term {
    Assertion(Assertion),
    Atom {
        atom: Atom,
        quantifier: Option<Quantifier>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Assertion {
    /// ```^```
    ///
    /// Matches the beginning of input. If the multiline flag is set to true, also matches immediately after a line break character.
    Beginning,
    /// ```$```
    ///
    /// Matches the end of input. If the multiline flag is set to true, also matches immediately before a line break character.
    End,
    /// ```\b```
    ///
    /// Matches a word boundary. This is the position where a word character is not followed or preceded by another word-character, such as between a letter and a space.
    WordBoundry,
    /// ```\B```
    ///
    /// Matches a non-word boundary. This is a position where the previous and next character are of the same type: Either both must be words, or both must be non-words
    NonWordBoundry,
    /// (?= Disjunction)
    LookAhead(Disjunction),
    /// (?! Disjunction)
    NegativeLookahead(Disjunction),
    /// (?<= Disjunction)
    LookBehind(Disjunction),
    /// (?<! Disjunction)
    NegativeLookBehind(Disjunction),
}

impl Assertion{
    pub fn is_lookahead(&self) -> bool{
        match self {
            Self::LookAhead(_) => true,
            Self::NegativeLookahead(_) => true,
            _ => false,
        }
    }
    pub fn is_lookbehind(&self) -> bool{
        match self {
            Self::LookBehind(_) => true,
            Self::NegativeLookBehind(_) => true,
            _ => false,
        }
    }
}

///  By default quantifiers like * and + are "greedy", meaning that they try to match as much of the string as possible.
///
/// The ? character after the quantifier makes the quantifier "non-greedy": meaning that it will stop as soon as it finds a match.
///
///  For example, given a string like "some \<foo> \<bar> new \</bar> \</foo> thing":
///
/// ```/<.*>/``` will match "\<foo> \<bar> new \</bar> \</foo>"
///
/// ```/<.*?>/``` will match "\<foo>"
#[derive(Debug, PartialEq, Eq)]
pub struct  Quantifier {
    pub min: u32,
    pub max: u32,
    pub greedy: bool,
}

impl Quantifier{

}

#[derive(Debug, PartialEq, Eq)]
pub enum Atom {
    Any,
    Character(char),

    CharacterClassEscape(CharacterClassEscape),
    CharacterClass(CharacterClassRanges),

    /// ( opt GroupSpecifier Disjunction )
    CapturingGroup{
        index: u32,
        disjunction: Disjunction
    },
    BackReferenceIndex{
        index: u32,
    },
    BackReferenceNamed{
        name: String
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum CharacterClassEscape {
    /// \d
    Digits,
    /// \D
    NonDigits,
    /// \s
    WhiteSpace,
    /// \S
    NonWhiteSpace,
    /// \w
    Alphanumeric,
    /// \W
    NonAlphanumeric,
    Property(UnicodePropertyValue),
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnicodePropertyValue {
    pub name: String,
    pub value: String
}


#[derive(Debug, PartialEq, Eq)]
pub struct CharacterClassRanges{
    pub is_negative: bool,
    pub ranges: Vec<ClassRange>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ClassRange{
    /// single character class range
    Char(char),
    /// a-z
    Range(char, char),
    Backspace,
    CharacterClassEscape(CharacterClassEscape),
}