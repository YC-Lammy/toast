
use alloc::borrow::Cow;
use alloc::string::ToString;

use pest_derive::Parser;

use pest::{Parser, iterators::Pair};

use crate::ast::*;

#[derive(Parser)]
#[grammar = "./regex.pest"]
pub struct RegexParser;

#[derive(Debug, Clone)]
pub struct Error{
    pub msg: Cow<'static, str>,
    pub span: (usize, usize),
}

impl Error{
    pub fn new<S:Into<Cow<'static, str>>>(msg: S, span: (usize, usize)) -> Self{
        Self { msg: msg.into(), span: span }
    }
}

impl core::fmt::Display for Error{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)?;
        f.write_str(", at offset: ")?;

        let mut buf = itoa::Buffer::new();

        f.write_str(buf.format(self.span.0))?;
        f.write_str("..")?;
        f.write_str(buf.format(self.span.1))?;

        Ok(())
    }
}

pub fn parse(input: &str, unicode_mode: bool) -> Result<Pattern, Error>{
    let re = RegexParser::parse(Rule::pattern, input);

    let pair = match re{
        Err(e) => {
            let span = match e.location{
                pest::error::InputLocation::Pos(p) => (p, p),
                pest::error::InputLocation::Span(p) => p
            };
            return Err(Error::new("invalid syntax", span))
        },
        Ok(mut pair) => {
            //debug_assert!(pair.len() == 1);
            pair.next().unwrap()
        },
    };

    let builder = ASTBuilder::default();

    builder.translate_pair_pattern(pair, unicode_mode)
}

#[derive(Debug, Default)]
struct ASTBuilder{
    groups: Vec<CaptureGroup>
}

impl ASTBuilder {
    pub fn translate_pair_pattern(mut self, pair: Pair<'_, Rule>, unicode_mode: bool) -> Result<Pattern, Error>{
        debug_assert!(pair.as_rule() == Rule::pattern);

        let disjunction = pair.into_inner().next().unwrap();

        let disjunction = self.translate_disjunction(disjunction, unicode_mode, true)?;

        return Ok(
            Pattern { 
                disjunction, 
                capture_groups: self.groups
            }
        )
    }

    fn translate_disjunction(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<Disjunction, Error>{
        debug_assert!(pair.as_rule() == Rule::disjunction);

        let mut alternatives = Vec::new();

        for alt in pair.into_inner(){
            alternatives.push(
                self.translate_alternative(alt, unicode_mode, n)?
            );
        }

        return Ok(
            Disjunction { alternatives: alternatives }
        )
    }

    fn translate_alternative(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<Alternative, Error>{
        debug_assert!(pair.as_rule() == Rule::alternative);

        let mut terms = Vec::new();

        for t in pair.into_inner(){
            terms.push(
                self.translate_term(t, unicode_mode, n)?
            );
        }

        return Ok(
            Alternative { terms: terms }
        )
    }

    fn translate_term(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<Term, Error>{
        debug_assert!(pair.as_rule() == Rule::term);

        for inner in pair.into_inner(){
            match inner.as_rule(){
                Rule::assertion => {
                    let asert = self.translate_assertion(inner, unicode_mode, n)?;

                    return Ok(Term::Assertion(asert))
                }
                Rule::atom => {
                    let atom = self.translate_atom(inner, unicode_mode, n)?;
                    
                    return Ok(Term::Atom { atom, quantifier: None })
                }
                Rule::atom_quantifier => {
                    let mut pairs = inner.into_inner();
                    let atom = self.translate_atom(pairs.next().unwrap(), unicode_mode, n)?;
                    let quant = self.translate_quantifier(pairs.next().unwrap(), unicode_mode, n)?;

                    return Ok(Term::Atom { atom: atom, quantifier: Some(quant) })
                }
                _ => unreachable!()
            }
        }

        unreachable!()
    }

    fn translate_assertion(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<Assertion, Error>{
        debug_assert!(pair.as_rule() == Rule::assertion);

        let child = pair.into_inner().next().unwrap();

        match child.as_rule(){
            Rule::assertion_beginning => {
                return Ok(Assertion::Beginning)
            }
            Rule::assertion_end => {
                return Ok(Assertion::End)
            }
            Rule::assertion_word_boundary => {
                return Ok(Assertion::WordBoundry)
            }
            Rule::assertion_not_word_boundary => {
                return Ok(Assertion::NonWordBoundry)
            }
            Rule::lookahead => {
                let disjunction = child.into_inner().next().unwrap();
                let disjunction = self.translate_disjunction(disjunction, unicode_mode, n)?;
                return Ok(Assertion::LookAhead(disjunction));
            }
            Rule::negative_lookahead => {
                let disjunction = child.into_inner().next().unwrap();
                let disjunction = self.translate_disjunction(disjunction, unicode_mode, n)?;
                return Ok(Assertion::NegativeLookahead(disjunction));
            }
            Rule::lookbehind => {
                let disjunction = child.into_inner().next().unwrap();
                let disjunction = self.translate_disjunction(disjunction, unicode_mode, n)?;
                return Ok(Assertion::LookBehind(disjunction));
            }
            Rule::negative_lookbehind => {
                let disjunction = child.into_inner().next().unwrap();
                let disjunction = self.translate_disjunction(disjunction, unicode_mode, n)?;
                return Ok(Assertion::NegativeLookBehind(disjunction));
            }
            _ => unreachable!()
        }
    }

    fn translate_quantifier(&mut self, pair: Pair<'_, Rule>, _unicode_mode: bool, _n: bool) -> Result<Quantifier, Error>{
        let s = pair.as_str();

        if s.starts_with("*"){
            let greedy = s.ends_with("?");
            return Ok(Quantifier { min: 0, max: u32::MAX, greedy })
        }

        if s.starts_with("+"){
            let greedy = s.ends_with("?");
            return Ok(Quantifier { min: 1, max: u32::MAX, greedy })
        }

        if s.starts_with("?"){
            let greedy = s.len() == 2;
            return Ok(Quantifier { min: 0, max: 1, greedy })
        }

        let child = pair.into_inner().next().unwrap();
        let greedy = child.as_str().ends_with("?");

        match child.as_rule(){
            Rule::quantifier_fixed_repeat => {
                let decimals = child.into_inner().next().unwrap();
                let n = decimals.as_str().parse::<u32>().unwrap();

                return Ok(Quantifier { min: n, max: n, greedy })
            }
            Rule::quantifier_repeat_atleast => {
                let decimals = child.into_inner().next().unwrap();
                let n = decimals.as_str().parse::<u32>().unwrap();

                return Ok(Quantifier { min: n, max: u32::MAX, greedy })
            }
            Rule::quantifier_repeat_in_range => {
                let span = (child.as_span().start(), child.as_span().end());
                let mut inner = child.into_inner();

                let decimals = inner.next().unwrap();
                let n = decimals.as_str().parse::<u32>().unwrap();

                let decimals = inner.next().unwrap();
                let m = decimals.as_str().parse::<u32>().unwrap();

                if n > m{
                    return Err(Error::new("Range out of order in quantifier", span));
                }

                return Ok(Quantifier { min: n, max: m, greedy })
            }
            _ => unreachable!()
        }
    }

    fn translate_atom(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<Atom, Error>{
        
        let s = pair.as_str();

        if s == "."{
            return Ok(Atom::Any)
        }

        if s.chars().count() == 1{
            return Ok(Atom::Character(s.chars().next().unwrap()))
        }

        let child = pair.into_inner().next().unwrap();

        match child.as_rule(){
            Rule::atom_escape => {
                return self.translate_atom_escape(child, unicode_mode, n)
            },
            Rule::character_class => {
                let c = self.translate_character_class(child, unicode_mode, n)?;
                return Ok(Atom::CharacterClass(c))
            }
            Rule::capture_group => {
                return self.translate_capture_group(child, unicode_mode, n)
            }
            Rule::non_capturing_group => {
                return self.translate_non_capturing_group(child, unicode_mode, n)
            }
            _ => unreachable!()
        }
    }

    fn translate_capture_group(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<Atom, Error>{
        debug_assert!(pair.as_rule() == Rule::capture_group);

        let mut inner = pair.into_inner();

        let mut name = None;
        let mut next = inner.next().unwrap();

        if next.as_rule() == Rule::group_name{
            name = Some(next.as_str().to_string());
            next = inner.next().unwrap();
        }

        debug_assert!(next.as_rule() == Rule::disjunction);

        let disjunction = self.translate_disjunction(next, unicode_mode, n)?;

        let index = self.groups.len();

        self.groups.push(
            CaptureGroup { 
                name: name.clone(), 
                is_capturing: true
            }
        );
        return Ok(Atom::CapturingGroup { 
            index: index as u32, 
            disjunction: disjunction
        })

    }

    fn translate_non_capturing_group(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<Atom, Error>{
        debug_assert!(pair.as_rule() == Rule::non_capturing_group);

        let child = pair.into_inner().next().unwrap();

        let disjunction = self.translate_disjunction(child, unicode_mode, n)?;

        let index = self.groups.len();

        self.groups.push(
            CaptureGroup { 
                name: None, 
                is_capturing: false
            }
        );

        return Ok(
            Atom::CapturingGroup { 
                index: index as u32,
                disjunction: disjunction
            }
        )
    }

    fn translate_atom_escape(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<Atom, Error>{
        debug_assert!(pair.as_rule() == Rule::atom_escape);

        let child = pair.into_inner().next().unwrap();

        match child.as_rule(){
            Rule::back_reference_index => {
                let index = u32::from_str_radix(child.as_str(), 10);

                let index = match index{
                    Ok(i) => i,
                    Err(_) => {
                        let span = child.as_span();
                        return Err(Error::new("Parse error: invalid number", (span.start(), span.end())))
                    }
                };

                return Ok(Atom::BackReferenceIndex { index: index })
            }
            Rule::group_name => {
                let name = child.as_str().to_string();

                return Ok(Atom::BackReferenceNamed { name: name })
            }
            Rule::class_escape => {
                let r = self.translate_class_escape(child, unicode_mode)?;

                return Ok(Atom::CharacterClassEscape(r));
            }
            Rule::character_escape => {
                let ch = self.translate_character_escape(child, unicode_mode, n)?;

                return Ok(Atom::Character(ch))
            }
            _ => unreachable!()
        }
    }

    fn translate_character_class(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<CharacterClassRanges, Error>{
        debug_assert!(pair.as_rule() == Rule::character_class);

        let mut negative = false;
        let mut ranges = Vec::new();

        let inner = pair.into_inner();

        for child in inner{
            if child.as_rule() == Rule::negative_character_class{
                negative = true;
            } else{
                debug_assert!(child.as_rule() == Rule::class_range);

                let r = self.translate_class_range(child, unicode_mode, n)?;

                ranges.push(r);
            }
        }

        return Ok(CharacterClassRanges{
            is_negative: negative,
            ranges: ranges
        })
    }

    fn translate_class_range(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<ClassRange, Error>{
        debug_assert!(pair.as_rule() == Rule::class_range);

        let span_start = pair.as_span().start();
        let span_end = pair.as_span().end();

        let s = pair.as_str();

        if s == "-"{
            return Ok(ClassRange::Char('-'))
        }

        let mut inner = pair.into_inner();

        // a range
        if inner.len() == 2{
            let start = self.translate_class_atom(inner.next().unwrap(), unicode_mode, n)?;
            let end = self.translate_class_atom(inner.next().unwrap(), unicode_mode, n)?;

            if start > end{
                return Err(Error::new("Range out of order in character class", (span_start, span_end)))
            }

            return Ok(
                ClassRange::Range(start, end)
            )
        } else{
            let child = inner.next().unwrap();

            if child.as_rule() == Rule::class_escape{
                let escape = self.translate_class_escape(child, unicode_mode)?;

                return Ok(ClassRange::CharacterClassEscape(escape))
            }

            if child.as_rule() == Rule::class_atom{
                let ch = self.translate_class_atom(child, unicode_mode, n)?;

                return Ok(ClassRange::Char(ch))
            }

            unreachable!()
        }
    }

    fn translate_class_atom(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, n: bool) -> Result<char, Error>{
        debug_assert!(pair.as_rule() == Rule::class_atom);

        let s = pair.as_str();

        if s == "\\b"{
            return Ok('\u{0008}')
        }
        if s == "\\-"{
            return Ok('-')
        }

        if s.starts_with("\\"){
            let char_escpae = pair.into_inner().next().unwrap();
            return self.translate_character_escape(char_escpae, unicode_mode, n);
        } else{
            let mut c = s.chars();
            return Ok(c.next().unwrap())
        }
    }

    fn translate_character_escape(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool, _n: bool) -> Result<char, Error>{
        debug_assert!(pair.as_rule() == Rule::character_escape);

        let s = pair.as_str();

        match s{
            "f" => return Ok('\u{000C}'),
            "n" => return Ok('\n'),
            "r" => return Ok('\r'),
            "t" => return Ok('\t'),
            "v" => return Ok('\u{000B}'),
            "0" => return Ok('\0'),
            _ => {}
        };


        if s.starts_with("c"){
            let control = (s.as_bytes()[1]) as u32;

            let code = if control >= 'a' as u32{
                control - 'a' as u32 + 1
            } else{
                control - 'A' as u32 + 1
            };
            
            return Ok(char::from_u32(code).unwrap())
        }

        let child = pair.into_inner().next().unwrap();

        match child.as_rule(){
            Rule::hex_escape => {
                let code = u32::from_str_radix(child.as_str(), 16).unwrap();

                let ch = match char::from_u32(code){
                    Some(c) => c,
                    None => return Err(Error::new("Invalide unicode code point", (child.as_span().start(), child.as_span().end())))
                };

                return Ok(ch)
            }
            // a utf16 surrogate pair
            Rule::unicode_surrogate_pair => {
                if !unicode_mode{
                    return Err(Error::new("Unicode surrogate pairs supported only when 'u' flag is set", (child.as_span().start(), child.as_span().end())))
                }

                // parse the unicode pair
                let ch = self.translate_unicode_surrogate_pair(child)?;
                
                return Ok(ch)
            }
            // a utf16 code unit
            Rule::unicode_code_unit => {
                if unicode_mode{

                }
                todo!()
            }
            Rule::unicode_code_point => {
                let span = child.as_span();

                if child.as_str().len() > 8{
                    return Err(Error::new("Invalid unicode code point", (span.start(), span.end())))
                }
                let code = u32::from_str_radix(child.as_str(), 16).unwrap();

                let ch = match char::from_u32(code){
                    Some(c) => c,
                    None => return Err(Error::new("Invalide unicode code point", (span.start(), span.end())))
                };

                return Ok(ch)
            }
            _ => {
                let s = child.as_str();
                debug_assert!(s.chars().count() == 1);

                let ch = s.chars().next().unwrap();

                return Ok(ch)
            }
        }
    }

    fn translate_unicode_surrogate_pair(&mut self, pair: Pair<'_, Rule>) -> Result<char, Error>{
        debug_assert!(pair.as_rule() == Rule::unicode_surrogate_pair);

        let span = pair.as_span();
        let mut inner = pair.into_inner();

        let lead = inner.next().unwrap();
        debug_assert!(lead.as_rule() == Rule::unicode_lead_surrogate);

        let lead = u16::from_str_radix(lead.as_str(), 16).unwrap();

        let trail = inner.next().unwrap();
        debug_assert!(trail.as_rule() == Rule::unicode_trail_surrogate);

        let trail = u16::from_str_radix(trail.as_str(), 16).unwrap();
        
        let re = char::decode_utf16([lead, trail]).next().unwrap();

        match re{
            Ok(c) => return Ok(c),
            Err(_) => {
                return Err(Error::new("Invalid unicode character", (span.start(), span.end())))
            }
        }
    }

    fn translate_class_escape(&mut self, pair: Pair<'_, Rule>, unicode_mode: bool) -> Result<CharacterClassEscape, Error>{
        debug_assert!(pair.as_rule() == Rule::class_escape);

        let s = pair.as_str();

        let r = 
        match s{
            "d" => CharacterClassEscape::Digits,
            "D" => CharacterClassEscape::NonDigits,
            "s" => CharacterClassEscape::WhiteSpace,
            "S" => CharacterClassEscape::NonWhiteSpace,
            "w" => CharacterClassEscape::Alphanumeric,
            "W" => CharacterClassEscape::NonAlphanumeric,
            _ => {

                if !unicode_mode{

                }
                let prop = pair.into_inner().next().unwrap();

                debug_assert!(prop.as_rule() == Rule::unicode_property);

                todo!()
            }
        };

        return Ok(r)
    }
}


#[test]
pub fn test(){
    let re = RegexParser::parse(Rule::pattern, "(?<=y)x|u").expect("parse error");
    let pair = re.into_iter().next().unwrap();

    let builder = ASTBuilder::default();

    let pat = builder.translate_pair_pattern(pair, true).unwrap();

    println!("{:#?}", pat);
}