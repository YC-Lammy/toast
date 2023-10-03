
#[derive(Debug)]
pub struct Pattern {
    pub disjunction: Disjunction,
    pub capture_groups: usize,
}

#[derive(Debug)]
pub struct Disjunction {
    pub alternatives: Vec<Alternative>,
}

#[derive(Debug)]
pub struct Alternative {
    pub terms: Vec<Term>,
}

#[derive(Debug)]
pub enum Term {
    Assertion(Assertion),
    Atom {
        atom: Atom,
        quantifier: Option<Quantifier>,
    },
}

#[derive(Debug)]
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

///  By default quantifiers like * and + are "greedy", meaning that they try to match as much of the string as possible.
///
/// The ? character after the quantifier makes the quantifier "non-greedy": meaning that it will stop as soon as it finds a match.
///
///  For example, given a string like "some \<foo> \<bar> new \</bar> \</foo> thing":
///
/// ```/<.*>/``` will match "\<foo> \<bar> new \</bar> \</foo>"
///
/// ```/<.*?>/``` will match "\<foo>"
#[derive(Debug)]
pub enum Quantifier {
    /// ```x*```
    ///
    /// Matches the preceding item "x" 0 or more times.
    Star { greedy: bool },
    /// ```x{ n }``` or ```x+```
    ///
    /// Where "n" is a positive integer, matches exactly "n" occurrences of the preceding item "x".
    Repeat { n: u32, greedy: bool },
    /// ```x{ n ,}```
    ///
    /// Where "n" is a positive integer, matches at least "n" occurrences of the preceding item "x".
    RepeatAtleast { n: u32, greedy: bool },
    /// ```x{ n, m }``` or ```x?```
    ///
    /// Where "n" is 0 or a positive integer, "m" is a positive integer, and m > n, matches at least "n" and at most "m" occurrences of the preceding item "x".
    RepeatLimited { n: u32, m: u32, greedy: bool },
}

impl Quantifier{
    pub fn min(&self) -> u32{
        match self{
            Self::Repeat { n, .. } => *n,
            Self::RepeatAtleast { n, .. } => *n,
            Self::RepeatLimited { n, .. } => *n,
            Self::Star { .. } => 0
        }
    }

    pub fn max(&self) -> u32{
        match self{
            Self::Repeat { .. } => u32::MAX,
            Self::RepeatAtleast { .. } => u32::MAX,
            Self::RepeatLimited { m, .. } => *m,
            Self::Star { .. } => u32::MAX
        }
    }

    pub fn greedy(&self) -> bool{
        match self{
            Self::Repeat { greedy, .. } => *greedy,
            Self::RepeatAtleast { greedy, .. } => *greedy,
            Self::RepeatLimited {  greedy, .. } => *greedy,
            Self::Star { greedy } => *greedy
        }
    }
}

#[derive(Debug)]
pub enum Atom {
    PatternCharacter(char),
    Character(Character),
    CharacterClass(CharacterClass),

    ClassRange(CharacterClassRanges),

    /// ( opt GroupSpecifier Disjunction )
    CapturingGroup{
        index: u32,
        name: Option<Box<[char]>>,
        disjunction: Disjunction
    },
    NonCapturingGroup{
        index: u32,
        disjunction: Disjunction,
    },
    BackReferenceIndex{
        index: u32,
    },
    BackReferenceNamed{
        name: Box<[char]>
    },
}

#[derive(Debug)]
pub enum CharacterClass {
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

#[derive(Debug)]
pub struct UnicodePropertyValue {
    pub name: Option<Box<[char]>>,
    pub value: Option<Box<[char]>>
}

#[derive(Debug)]
pub enum Character{
    /// ```.```
    Any,
    /// ```\f```
    FormFeed,
    /// ```\r```
    Carriage,
    /// ```\n```
    LineFeed,
    /// ```\t```
    HorizontalTab,
    /// ```\v```
    VerticalTab,
    /// ```\cX```
    ControlChar(char),
    Char(char),
} 

#[derive(Debug)]
pub struct CharacterClassRanges{
    pub is_negative: bool,
    pub ranges: Vec<ClassRange>,
}

#[derive(Debug)]
pub enum ClassRange{
    /// single character class range
    Char(char),
    /// -a
    To(char),
    /// a-
    From(char),
    /// a-z
    Range(char, char),
    CharacterClass(CharacterClass),
    Character(Character),
    Backspace,
}