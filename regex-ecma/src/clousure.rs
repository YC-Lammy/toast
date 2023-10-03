use std::sync::Arc;

use crate::ast::*;


pub struct MatchState<'a>{
    pub input: &'a str,
    pub end_index: usize,
    /// would be (0, 0) if uninitialised
    pub captures: Vec<(usize, usize)>,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct MatchResult(i64);

impl MatchResult{
    pub const FAILURE: MatchResult = MatchResult(-1);

    pub fn is_failure(&self) -> bool{
        self.0 == -1
    }

    pub fn is_success(&self) -> bool{
        self.0 >= 0
    }
}

type MatcherContinuation = Arc<dyn Fn(&mut MatchState) -> MatchResult>;
type Matcher = Arc<dyn Fn(&mut MatchState, MatcherContinuation) -> MatchResult>;

pub struct Generator{
    ignore_case: bool,
    multiline: bool,
    dot_all: bool,
    unicode: bool,
}

impl Generator{
    pub fn translate_pattern(&mut self, pattern: &Pattern) -> Arc<dyn Fn(&str, usize) -> MatchResult>{

        let m = self.translate_disjunction(&pattern.disjunction, false);

        fn continuation(state: &mut MatchState) -> MatchResult{
            return MatchResult(state.end_index as i64);
        }
        let c:MatcherContinuation = Arc::new(continuation);

        let cap = pattern.capture_groups;

        return Arc::new(
            move |input, end_index|{
                let mut state = MatchState{
                    input,
                    end_index,
                    captures: Vec::new(),
                };

                state.captures.resize(cap, (0, 0));

                return m(&mut state, c.clone())
            }
        )
    }

    pub fn translate_disjunction(&mut self, disjunction: &Disjunction, is_backward_dir: bool) -> Matcher{
        if disjunction.alternatives.len() == 0{
            return Arc::new(
                move |state, c|{
                    return c(state)
                }
            );
        };

        if disjunction.alternatives.len() == 1{
            let m = self.translate_alternative(&disjunction.alternatives[0], is_backward_dir);

            return Arc::new(
                move |state, c|{
                    return m(state, c)
                }
            )
        };

        let mut alts = Vec::new();

        for alt in &disjunction.alternatives{
            alts.push(self.translate_alternative(alt, is_backward_dir));
        };

        return Arc::new(
            move |state, c|{
                for alt in &alts{
                    let re = alt(state, c.clone());
                    if re.is_success(){
                        return re;
                    }
                }
                return MatchResult::FAILURE
            }
        );
    }

    pub fn translate_alternative(&mut self, alternative: &Alternative, is_backward_dir: bool) -> Matcher{
        if alternative.terms.is_empty(){
            return Arc::new(
                move |state, c|{
                    return c(state)
                }
            )
        };

        if alternative.terms.len() == 1{
            let term = self.translate_term(&alternative.terms[0], is_backward_dir);

            return Arc::new(
                move |state, c|{
                    return term(state, c)
                }
            )
        }

        let mut first_term:Option<Matcher> = None;
        let mut terms = Vec::new();

        if !is_backward_dir{
            for t in alternative.terms.iter(){
                
                let m = self.translate_term(t, is_backward_dir);

                if first_term.is_none(){
                    first_term = Some(m);
                } else{
                    terms.push(m);
                }
            }
        } else{
            for t in alternative.terms.iter().rev(){
                
                let m = self.translate_term(t, is_backward_dir);

                if first_term.is_none(){
                    first_term = Some(m);
                } else{
                    terms.push(m);
                }
            }
        }

        terms.reverse();

        let first_term = first_term.unwrap();

        return Arc::new(
            move |state, c|{
                let mut c = c;

                for t in &terms{
                    let t = t.clone();

                    c = Arc::new(
                        move |state|{
                            return t(state, c.clone())
                        }
                    );
                }

                return first_term(state, c);
            }
        );
    }

    pub fn translate_term(&mut self, term: &Term, is_backward_dir: bool) -> Matcher{
        match term{
            Term::Assertion(a) => {
                return self.translate_assertion(a);
            },
            Term::Atom { atom, quantifier } => {
                if let Some(quantifier) = quantifier{
                    let m = self.translate_atom(atom, is_backward_dir);

                    let min = quantifier.min();
                    let max = quantifier.max();
                    let greedy = quantifier.greedy();

                    let paren_index = todo!();
                    let paren_count = todo!();

                    return Arc::new(
                        move |state, c|{
                            return repeat_matcher(m.clone(), min, max, greedy, state, c, paren_index, paren_count)
                        }
                    )
                } else{

                    return self.translate_atom(atom, is_backward_dir);
                }
            }
        }
    }

    pub fn translate_assertion(&mut self, assertion: &Assertion) -> Matcher{
        match assertion{
            Assertion::Beginning => {
                if self.multiline{
                    // matches beginning of line
                    return Arc::new(
                        move |state, c|{
                            if state.end_index == 0{
                                return c(state)
                            }

                            return MatchResult::FAILURE
                        }
                    )
                } else{
                    // only matches when index == 0
                    return Arc::new(
                        move |state, c|{
                            if state.end_index == 0{
                                return c(state)
                            }
                            return MatchResult::FAILURE
                        }
                    )
                }
                
            },
            Assertion::End => {
                let is_multiline = self.multiline;

                return Arc::new(
                    move|state, c|{
                        let e = state.end_index;
                        let input_length = state.input.len();

                        if input_length == state.end_index{
                            return c(state);
                        }

                        if is_multiline{
                            let s = state.input.as_bytes()[e];
                            if s == '\n' as u8{
                                return c(state)
                            }
                        }

                        return MatchResult::FAILURE
                    }
                )
            },
            Assertion::WordBoundry => {
                return Arc::new(
                    move |state, c|{
                        todo!()
                    }
                )
            },
            _ => todo!()
        }
    }

    pub fn translate_atom(&mut self, atom: &Atom, is_backward_dir: bool) -> Matcher{
        todo!()
    }
}

pub fn repeat_matcher(
    m: Matcher, 
    min: u32, 
    max: u32, 
    greedy: bool, 
    state: &mut MatchState, 
    c: MatcherContinuation, 
    paren_index: usize,
    paren_count: usize,
) -> MatchResult{
    
    if max == 0{
        return c(state);
    }

    let m2 = m.clone();

    let x_end_index = state.end_index;

    let d: MatcherContinuation = Arc::new(
        move |state|{
            if min == 0 && state.end_index == x_end_index{
                return MatchResult::FAILURE
            }
            let min2 = 
            if min == 0{
                0
            } else{
                min -1
            };

            let max2 = if max == u32::MAX{
                u32::MAX
            } else{
                max - 1
            };

            return repeat_matcher(m2, min2, max2, greedy, state, c, paren_index, paren_count)
        }
    );

    let mut cap = state.captures.clone();

    for k in paren_index + 1 ..= paren_index + paren_count{
        cap[k] = (0, 0);
    }

    let mut xr = MatchState{
        input: state.input,
        end_index: state.end_index,
        captures: cap
    };

    if min != 0{
        return m(&mut xr, d)
    }

    if !greedy{
        let z = c(state);
        if !z.is_failure(){
            return z;
        }

        return m(&mut xr, d)
    }

    let z = m(&mut xr, d);
    if !z.is_failure(){
        return z
    }

    return c(state);
}