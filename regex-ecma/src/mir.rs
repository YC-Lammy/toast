use core::ops::RangeInclusive;

use crate::ast::*;

#[derive(Debug, Clone)]
pub enum RegexIR{
    StartAlt,
    EndAlt,

    Repeat{
        min: u32,
        max: u32,
        greedy: bool,
    },
    EndRepeat,

    /// match a beginning, if multiline, match line start
    LineStart,
    NotLineStart,
    /// match an end, if multiline, match line end
    LineEnd,
    /// matches when not end, excludes terminator if multiline
    NotLineEnd,

    /// match a word boundary
    WordBoundary,
    /// match not a word boundary
    NotWordBoundary,

    /// match a character
    Char(char),
    /// not a character
    NotChar(char),

    /// match a range of characters
    Range(Box<[RangeInclusive<char>]>),
    NegativeRange(Box<[RangeInclusive<char>]>),

    /// matches any character
    Any{
        include_line_terminater: bool
    },
    
    /// match a reference from capture group
    BackReference{
        capture_group: u32,
    },
    /// match a reference from capture group backwards
    BackwardBackReference{
        capture_group: u32,
    },

    /// returns true when the back reference does not match
    NegativeBackReference{
        capture_group: u32,
    },
    /// returns true when the back reference does not match backwards
    NegativeBackwardBackReference{
        capture_group: u32
    },

    /// fetch the next charater and increment the cursor by one character
    IncrementCursor,
    /// fetch the previous character and decrement the cursor by one character
    DecrementCursor,

    /// push current cursor onto stack
    PushCharPos,
    /// restore cursor pop from stack
    RestoreCharPos,
    /// swap the top of stack with the current position
    SwapCharPos,

    /// save current cursor to capture group
    SaveStartPos{
        capture_group: u32
    },
    /// save current cursor to capture group
    SaveEndPos{
        capture_group: u32,
    },

    AlwaysTrue,
    AlwaysFalse
}

pub struct IRGenerator{
    ignore_case: bool,
    dotall: bool,
    multiline: bool,
    unicode: bool,

    pub(crate) ir: Vec<RegexIR>
}

impl IRGenerator {
    pub fn new(i: bool, m: bool, s: bool, u: bool) -> Self{
        Self { 
            ignore_case: i,
            dotall: s, 
            multiline: m, 
            unicode: u, 
            ir: Vec::new()
        }
    }

    pub fn translate_pattern(&mut self, pattern: &Pattern){

        self.translate_disjunction(&pattern.disjunction, false, false);
    }

    pub fn translate_disjunction(&mut self, disjunction: &Disjunction, negative: bool, backwards: bool){
        for alt in &disjunction.alternatives{
            self.ir.push(RegexIR::StartAlt);

            self.translate_alternative(alt, negative, backwards);

            self.ir.push(RegexIR::EndAlt);
        }
    }

    pub fn translate_alternative(&mut self, alt: &Alternative, negative: bool, backwards: bool){
        let mut reverse_looks = Vec::new();

        if alt.terms.is_empty(){
            self.ir.push(RegexIR::AlwaysTrue);
        }

        if backwards{
            for term in alt.terms.iter().rev(){
                // if it is lookbehind, add it to queue
                if let Term::Assertion(a) = term{
                    if a.is_lookahead(){
                        reverse_looks.push(term);

                        // save the current position
                        self.ir.push(RegexIR::PushCharPos);

                        // skip to next term
                        continue;
                    }
                }
                
                while let Some(lookahead) = reverse_looks.pop(){
                    self.ir.push(RegexIR::SwapCharPos);

                    self.translate_term(&lookahead, negative, backwards);

                    self.ir.push(RegexIR::RestoreCharPos);
                };

                self.translate_term(term, negative, true);
            }

        } else{

            for term in &alt.terms{

                // if it is lookbehind, add it to queue
                if let Term::Assertion(a) = term{
                    if a.is_lookbehind(){
                        reverse_looks.push(term);

                        // save the current position
                        self.ir.push(RegexIR::PushCharPos);

                        continue;
                    }
                }
                
                while let Some(lookbehind) = reverse_looks.pop(){
                    self.ir.push(RegexIR::SwapCharPos);

                    self.translate_term(&lookbehind, negative, backwards);

                    self.ir.push(RegexIR::RestoreCharPos);
                };

                self.translate_term(term, negative, false);
            }
        }
        
    }

    pub fn translate_term(&mut self, term: &Term, negative: bool, backwards: bool){
        match term {
            Term::Assertion(a) => {
                self.translate_assertion(negative, a);
            },
            Term::Atom { atom, quantifier } => {
                if let Some(q) = quantifier{
                    self.ir.push(RegexIR::Repeat { min: q.min, max: q.max, greedy: q.greedy });
                }

                self.translate_atom(atom, negative, backwards);

                if let Some(_) = quantifier{
                    self.ir.push(RegexIR::EndRepeat);
                }
            }
        }
    }

    pub fn translate_assertion(&mut self, negative: bool, assertion: &Assertion){
        match assertion {
            Assertion::Beginning => {
                if negative{
                    self.ir.push(RegexIR::NotLineStart);
                } else{
                    self.ir.push(RegexIR::LineStart);
                }
            }
            Assertion::End => {
                if negative{
                    self.ir.push(RegexIR::NotLineEnd);
                } else{
                    self.ir.push(RegexIR::LineEnd);
                }
            }

            Assertion::WordBoundry => {
                if negative{
                    self.ir.push(RegexIR::NotWordBoundary);
                } else{
                    self.ir.push(RegexIR::WordBoundary);
                }
            }
            Assertion::NonWordBoundry => {
                if negative{
                    self.ir.push(RegexIR::WordBoundary);
                } else{
                    self.ir.push(RegexIR::NotWordBoundary);
                }
            }
            Assertion::LookAhead(disjunction) => {
                self.ir.push(RegexIR::PushCharPos);

                self.translate_disjunction(disjunction, negative, false);

                self.ir.push(RegexIR::RestoreCharPos);
            }
            Assertion::NegativeLookahead(disjunction) => {
                self.ir.push(RegexIR::PushCharPos);

                // if it is a negative match, negative lookahead becomes positive lookahead.
                let negative = !negative;

                self.translate_disjunction(disjunction, negative, false);

                self.ir.push(RegexIR::RestoreCharPos);
            },

            Assertion::LookBehind(disjunction) => {
                self.ir.push(RegexIR::PushCharPos);

                self.translate_disjunction(disjunction, negative, true);

                self.ir.push(RegexIR::RestoreCharPos);
            }

            Assertion::NegativeLookBehind(disjunction) => {
                self.ir.push(RegexIR::PushCharPos);

                // if it is a negative match, negative lookbehind becomes positive lookbehind.
                let negative = !negative;

                self.translate_disjunction(disjunction, negative, true);

                self.ir.push(RegexIR::RestoreCharPos);
            }
        }
    }

    pub fn translate_atom(&mut self, atom: &Atom, negative: bool, backwards: bool){
        match atom {
            Atom::Character(p) => {
                if backwards{
                    self.ir.push(RegexIR::DecrementCursor);
                } else{
                    self.ir.push(RegexIR::IncrementCursor);
                }

                if negative{
                    self.ir.push(RegexIR::NotChar(*p));
                } else{
                    self.ir.push(RegexIR::Char(*p));
                }
            }
            Atom::Any => {
                // not match any if negative
                if negative{
                    if self.dotall{
                        // does not match anything
                        self.ir.push(RegexIR::AlwaysFalse);
                    } else{
                        // only matches line end
                        self.ir.push(RegexIR::Range(Box::new(['\n'..='\n', '\r'..='\r', '\u{2028}'..='\u{2029}'])));
                    }
                } else{
                    // match any, include line terminator if dotall flag is set
                    self.ir.push(RegexIR::Any { include_line_terminater: self.dotall });
                };
            },
            Atom::CapturingGroup { index, disjunction} => {

                // save the end position if backwards
                if backwards{
                    self.ir.push(RegexIR::SaveEndPos { capture_group: *index });
                } else{
                    // save the start position
                    self.ir.push(RegexIR::SaveStartPos { capture_group: *index });
                };

                // translate the disjunction
                self.translate_disjunction(disjunction, negative, backwards);

                // save the start position if backwards
                if backwards{
                    self.ir.push(RegexIR::SaveStartPos { capture_group: *index });
                } else{
                    // save the start position
                    self.ir.push(RegexIR::SaveEndPos { capture_group: *index });
                };
            },
            Atom::CharacterClass(r) => self.translate_class_ranges(r, negative, backwards),
            Atom::CharacterClassEscape(c) => self.translate_character_class_escape(c, negative, backwards),

            Atom::BackReferenceIndex { index } => {
                if backwards && negative{
                    self.ir.push(RegexIR::NegativeBackwardBackReference { capture_group: *index })
                } else if negative{
                    self.ir.push(RegexIR::NegativeBackReference { capture_group: *index });
                } else if backwards{
                    self.ir.push(RegexIR::BackwardBackReference { capture_group: *index });
                } else{
                    self.ir.push(RegexIR::BackReference { capture_group: *index });
                }
            }
            Atom::BackReferenceNamed { name } => {
                todo!()
            }
        }
    }

    pub fn translate_class_ranges(&mut self, r: &CharacterClassRanges, negative: bool, backwards: bool){

        if backwards{
            self.ir.push(RegexIR::DecrementCursor);
        } else{
            self.ir.push(RegexIR::IncrementCursor);
        }

        let mut ranges = Vec::new();

        let negative =  negative ^ r.is_negative;

        for range in &r.ranges{
            match range{
                ClassRange::Backspace => {
                    ranges.push('\u{0008}'..='\u{0008}');
                },
                ClassRange::Char(c) => {
                    ranges.push(*c..=*c);
                },
                ClassRange::Range(a, b) => {
                    ranges.push(*a..=*b);
                }
                ClassRange::CharacterClassEscape(charclass) => {
                    match charclass{
                        CharacterClassEscape::Alphanumeric => {
                            ranges.push('A'..='Z');
                            ranges.push('a'..='z');
                            ranges.push('0'..='9');
                            ranges.push('_'..='_');
                        }
                        CharacterClassEscape::Digits => {
                            ranges.push('0'..='9');
                        }
                        CharacterClassEscape::NonAlphanumeric => {
                            ranges.push('\u{0000}'..='/');
                            ranges.push(':'..='@');
                            ranges.push('['..='^');
                            ranges.push('`'..='`');
                            ranges.push('{'..=char::MAX);
                        },
                        CharacterClassEscape::NonDigits => {
                            ranges.push('\u{0000}'..='/');
                            ranges.push(':'..=char::MAX);
                        }
                        CharacterClassEscape::NonWhiteSpace => {
                            ranges.push('\u{0000}'..='\u{0008}');
                            ranges.push('\u{000E}'..='\u{0019}');
                            ranges.push('\u{0021}'..='\u{009F}');
                            ranges.push('\u{00a1}'..='\u{167F}');
                            ranges.push('\u{1681}'..='\u{1FFF}');
                            ranges.push('\u{200B}'..='\u{2027}');
                            ranges.push('\u{202A}'..='\u{202E}');
                            ranges.push('\u{2030}'..='\u{205E}');
                            ranges.push('\u{2060}'..='\u{2FFF}');
                            ranges.push('\u{3001}'..='\u{FEFE}');
                            ranges.push('\u{FF00}'..=char::MAX);
                        },
                        CharacterClassEscape::WhiteSpace => {
                            ranges.push('\u{0009}'..='\u{000D}');
                            ranges.push('\u{0020}'..='\u{0020}');
                            ranges.push('\u{00A0}'..='\u{00A0}');
                            ranges.push('\u{1680}'..='\u{1680}');
                            ranges.push('\u{2000}'..='\u{200A}');
                            ranges.push('\u{2028}'..='\u{2029}');
                            ranges.push('\u{202F}'..='\u{202F}');
                            ranges.push('\u{205F}'..='\u{205F}');
                            ranges.push('\u{3000}'..='\u{3000}');
                            ranges.push('\u{FEFF}'..='\u{FEFF}');
                        }
                        CharacterClassEscape::Property(p) => {
                            todo!()
                        }
                    }
                }
            }
        };

        if negative{
            self.ir.push(RegexIR::NegativeRange(ranges.into()));
        } else{
            self.ir.push(
                RegexIR::Range(ranges.into())
            );
        };
    }

    pub fn translate_character_class_escape(&mut self, charclass: &CharacterClassEscape, negative: bool, backwards: bool){
        let mut ranges = Vec::new();

        if backwards{
            self.ir.push(RegexIR::DecrementCursor);
        } else{
            self.ir.push(RegexIR::IncrementCursor);
        }

        match charclass{
            CharacterClassEscape::Alphanumeric => {
                ranges.push('A'..='Z');
                ranges.push('a'..='z');
                ranges.push('0'..='9');
                ranges.push('_'..='_');
            }
            CharacterClassEscape::Digits => {
                ranges.push('0'..='9');
            }
            CharacterClassEscape::NonAlphanumeric => {
                ranges.push('\u{0000}'..='/');
                ranges.push(':'..='@');
                ranges.push('['..='^');
                ranges.push('`'..='`');
                ranges.push('{'..=char::MAX);
            },
            CharacterClassEscape::NonDigits => {
                ranges.push('\u{0000}'..='/');
                ranges.push(':'..=char::MAX);
            }
            CharacterClassEscape::NonWhiteSpace => {
                ranges.push('\u{0000}'..='\u{0008}');
                ranges.push('\u{000E}'..='\u{0019}');
                ranges.push('\u{0021}'..='\u{009F}');
                ranges.push('\u{00a1}'..='\u{167F}');
                ranges.push('\u{1681}'..='\u{1FFF}');
                ranges.push('\u{200B}'..='\u{2027}');
                ranges.push('\u{202A}'..='\u{202E}');
                ranges.push('\u{2030}'..='\u{205E}');
                ranges.push('\u{2060}'..='\u{2FFF}');
                ranges.push('\u{3001}'..='\u{FEFE}');
                ranges.push('\u{FF00}'..=char::MAX);
            },
            CharacterClassEscape::WhiteSpace => {
                ranges.push('\u{0009}'..='\u{000D}');
                ranges.push('\u{0020}'..='\u{0020}');
                ranges.push('\u{00A0}'..='\u{00A0}');
                ranges.push('\u{1680}'..='\u{1680}');
                ranges.push('\u{2000}'..='\u{200A}');
                ranges.push('\u{2028}'..='\u{2029}');
                ranges.push('\u{202F}'..='\u{202F}');
                ranges.push('\u{205F}'..='\u{205F}');
                ranges.push('\u{3000}'..='\u{3000}');
                ranges.push('\u{FEFF}'..='\u{FEFF}');
            }
            CharacterClassEscape::Property(p) => {
                todo!()
            }
        }

        if negative{
            self.ir.push(RegexIR::NegativeRange(ranges.into()));
        } else{
            self.ir.push(RegexIR::Range(ranges.into()));
        }
    }
}