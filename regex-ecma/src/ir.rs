

use crate::ast::*;

/// referenced from libregexp in quickjs
#[derive(Debug, Clone, PartialEq)]
pub enum RegexIR {
    /// never used
    Invalid,
    Char(u8),
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
    /// reset save positions
    SaveReset(u16),
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
    Range(Box<[core::ops::Range<u8>]>),
    /// variable length
    Range32(Box<[std::ops::Range<u32>]>),
    Lookahead(u32),
    NegativeLookahead(u32),
    /// push the character position on the stack
    PushCharPos,
    /// pop one stack element and jump if equal to the character
    BNECharPos(u32),
    /// go to the previous char
    Prev,
    SimpleGreedyQuant(u128),
}


pub struct IRGenerator{
    dotall: bool,
    multiline: bool,
    unicode: bool,

    capture_count: usize,

    last_atom_start: i64,
    last_capture_count: i64,

    ir: Vec<RegexIR>,
}

impl IRGenerator{
    pub fn translate_pattern(&self, pattern: &Pattern){

    }

    fn translate_disjunction(&mut self, disjunction: &Disjunction, is_backward_dir: bool){
        for alt in &disjunction.alternatives{
            todo!()
        }
    }

    fn translate_alternative(&mut self, alternative: &Alternative, is_backward_dir: bool){
        
    }

    fn translate_term(&mut self, term: &Term, is_backward_dir: bool){
        self.last_atom_start = -1;
        self.last_capture_count = 0;

        match term{
            Term::Assertion(assertion) => {
                self.translate_assertion(assertion, is_backward_dir);
            }
            Term::Atom { atom, quantifier } => {

            }
        }
    }

    fn translate_assertion(&mut self, assertion: &Assertion, is_backward_dir: bool){
        let mut is_neg = false;
        let mut is_backward_lookahead = false;

        let d = match assertion{
            Assertion::Beginning => {
                self.ir.push(RegexIR::LineStart);
                return;
            },
            Assertion::End => {
                self.ir.push(RegexIR::LindEnd);
                return;
            }
            Assertion::WordBoundry => {
                self.ir.push(RegexIR::WordBoundary);
                return;
            }
            Assertion::NonWordBoundry => {
                self.ir.push(RegexIR::NotWordBoundary);
                return;
            }
            Assertion::LookAhead(d) => {
                is_neg = false;
                is_backward_lookahead = false;
                d
            }
            Assertion::NegativeLookahead(d) => {
                is_neg = true;
                is_backward_lookahead = false;
                d
            }
            Assertion::LookBehind(d) => {
                is_neg = false;
                is_backward_lookahead = true;
                d
            }
            Assertion::NegativeLookBehind(d) => {
                is_neg = true;
                is_backward_lookahead = true;
                d
            }
        };

        if !is_backward_dir{
            self.last_atom_start = self.ir.len() as i64;
            self.last_capture_count = self.capture_count as i64;
        }

        let pos = self.ir.len();
        if is_neg{
            self.ir.push(RegexIR::NegativeLookahead(0))
        } else{
            self.ir.push(RegexIR::Lookahead(0))
        }

        self.translate_disjunction(d, is_backward_lookahead);

        self.ir.push(RegexIR::Match);

        let size = (self.ir.len() - pos) as u32;

        if is_neg{
            self.ir[pos] = RegexIR::NegativeLookahead(size);
        } else{
            self.ir[pos] = RegexIR::Lookahead(size);
        }
    }

    fn translate_atom(&mut self, atom: &Atom, is_backward_dir: bool){
        match atom{
            Atom::Character(c) => {
                self.translate_character(c, is_backward_dir);
            },
            Atom::NonCapturingGroup { index, disjunction } => {
                self.last_atom_start = self.ir.len() as i64;
                self.last_capture_count = self.capture_count as i64;

                self.translate_disjunction(disjunction, is_backward_dir);  
            }
            Atom::CapturingGroup { 
                index, 
                name, 
                disjunction 
            } => {
                self.last_atom_start = self.ir.len() as i64;
                self.last_capture_count = self.capture_count as i64;

                self.capture_count += 1;

                let capture_index = *index;

                if is_backward_dir{
                    self.ir.push(RegexIR::SaveEnd(capture_index))
                } else{
                    self.ir.push(RegexIR::SaveStart(capture_index))
                }
                
                self.translate_disjunction(disjunction, is_backward_dir);

                if is_backward_dir{
                    self.ir.push(RegexIR::SaveStart(capture_index));
                } else{
                    self.ir.push(RegexIR::SaveEnd(capture_index));
                }
            }
            Atom::BackReferenceNamed { name } => {
                self.last_atom_start = self.ir.len() as i64;
                self.last_capture_count = self.capture_count as i64;

                if is_backward_dir{
                    self.ir.push(RegexIR::BackwardBackReference());
                } else{
                    self.ir.push(RegexIR::BackReference());
                }
            }
            Atom::BackReferenceIndex { index } => {
                self.last_atom_start = self.ir.len() as i64;
                self.last_capture_count = self.capture_count as i64;

                if is_backward_dir{
                    self.ir.push(RegexIR::BackwardBackReference(*index));
                } else{
                    self.ir.push(RegexIR::BackReference(*index));
                }
            }
            Atom::PatternCharacter(c) => {
                if is_backward_dir{
                    self.ir.push(RegexIR::Prev);
                }

                
            }
        }
    }

    fn translate_character(&mut self, c: &Character, is_backward_dir: bool){
        match c{
            Character::Any => {
                self.last_atom_start = self.ir.len() as i64;
                self.last_capture_count = self.capture_count as i64;

                if is_backward_dir{
                    self.ir.push(RegexIR::Prev);
                }

                if self.dotall{
                    self.ir.push(RegexIR::Any);
                } else{
                    self.ir.push(RegexIR::Dot)
                };

                if is_backward_dir{
                    self.ir.push(RegexIR::Prev);
                }
                
            },
        }
    }
}