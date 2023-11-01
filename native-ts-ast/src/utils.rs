use pegy::{Error, Span, Parse};
use pegy::util::{
    ANY,
    RepeatQuiet,
    Recursive
};


#[derive(Debug, Default)]
pub struct LineTerminator;


impl pegy::Parse for LineTerminator{
    type Output = Self;
    async fn parse<S:pegy::Source>(src: &mut S) -> Result<Self::Output, pegy::Error> {
        let pos = src.current_position();

        if let Some(c) = src.peek().await{
            if c.ch == '\u{000A}' || c.ch == '\u{000D}' || c.ch == '\u{2028}' || c.ch == '\u{2019}'{
                src.set_position(pos + c.length);
                return Ok(Self)
            }
        }
        
        return Err(Error::new(Span::new(pos, pos), "expected line terminator"))
    }
}

#[derive(Debug, Default)]
pub struct WhiteSpaces;


impl pegy::Parse for WhiteSpaces{
    type Output = Self;
    async fn parse<S:pegy::Source>(src: &mut S) -> Result<Self::Output, Error> {
        while let Some(c) = src.peek().await{
            let pos = src.current_position();
            match c.ch{
                '\u{0009}' |
                '\u{000B}' |
                '\u{000c}' |
                '\u{FEFF}' |
                '\u{0020}' |
                '\u{00A0}' |
                '\u{1680}' |
                '\u{2000}'..='\u{200A}'|
                '\u{202F}' |
                '\u{205F}' |
                '\u{3000}' => {
                    src.set_position(pos + c.length);
                    continue;
                }
                _ => break,
            }
        };

        return Ok(Self)
    }
}

#[derive(Debug, Default, Parse)]
#[grammar("//" (!LineTerminator ANY)*)]
pub struct Comment;

#[derive(Debug, Default, Parse)]
#[grammar("await" | "break" | "case" | "catch" | "class" | "const" | "continue" | "debugger" | "default" | "delete" | "do" | "else" | "enum" | "export" | "extends" | "false" | "finally" | "for" | "function" | "if" | "import" | "in" | "instanceof" | "new" | "null" | "return" | "super" | "switch" | "this" | "throw" | "true" | "try" | "typeof" | "var" | "void" | "while" | "with" | "yield")]
pub struct ReservedWord;

#[derive(Debug, Default, Parse)]
#[grammar($item0:(IdentifierStart RepeatQuiet<IdentifierPart>))]
pub struct IdentifierName(pub Span);

#[derive(Debug, Default)]
pub struct IdentifierStart;

impl pegy::Parse for IdentifierStart{
    type Output = Self;
    async fn parse<S:pegy::Source>(src: &mut S) -> Result<Self::Output, Error> {
        let pos = src.current_position();
        if let Some(c) = src.peek().await{
            if c.ch == '$' || c.ch == '_'{
                src.set_position(pos + c.length);
                return Ok(Self)
            }

            if unicode_id_start::is_id_start(c.ch){
                src.set_position(pos + c.length);
                return Ok(Self)
            }
        }

        return Err(Error::new(Span::new(pos, pos), "expected identifier"))
    }
}

#[derive(Debug, Default)]
pub struct IdentifierPart;

impl pegy::Parse for IdentifierPart{
    type Output = Self;
    async fn parse<S:pegy::Source>(src: &mut S) -> Result<Self::Output, Error> {
        let pos = src.current_position();
        if let Some(c) = src.peek().await{
            if c.ch == '$' || c.ch == '\u{200C}' || c.ch == '\u{200D}'{
                src.set_position(pos + c.length);
                return Ok(Self)
            }

            if unicode_id_start::is_id_continue(c.ch){
                src.set_position(pos + c.length);
                return Ok(Self)
            }
        }

        return Err(Error::new(Span::new(pos, pos), "expected identifier"))
    }
}


#[derive(Debug, Default, Parse)]
#[grammar('_')]
pub struct NumericLiteralSeparator;

#[derive(Debug)]
pub enum NumericLiteral{
    Number(f64),
    BigInt(i128),
}

impl Default for NumericLiteral{
    fn default() -> Self {
        Self::Number(f64::NAN)
    }
}

impl Parse for NumericLiteral{
    type Output = Self;
    async fn parse<S:pegy::Source>(src: &mut S) -> Result<Self::Output, Error> {
        let start = src.current_position();

        // bin
        if src.match_str("0b").await{
            let mut i = 0.0f64;

            while let Some(c) = src.peek().await{
                let pos = src.current_position();

                match c.ch{
                    '0' => {
                        i = i * 2.0;
                    }
                    '1' => {
                        i = i * 2.0 + 1.0;
                    }
                    _ => break,
                };

                src.set_position(pos + c.length);
            };
            return Ok(NumericLiteral::Number(i));
            
        // hex
        } else if src.match_str("0x").await{
            let mut i = 0.0f64;

            while let Some(c) = src.peek().await{
                let pos = src.current_position();

                if let Some(n) = c.ch.to_digit(16){
                    i = i * 16.0 + n as f64;

                    src.set_position(pos + c.length);
                } else{
                    break;
                }
            };
            return Ok(NumericLiteral::Number(i));

        } else if src.match_char('.').await{
            // decimal literal

            let mut i: f64 = 0.0;
            let mut d: i32 = 0;
            while let Some(c) = src.match_char_range('0'..='9').await{
                d += 1;

                let n = (c as u32 - '0' as u32) as f64;
                i = i + n / (10.0f64.powi(d));
            };

            if d == 0{
                let end = src.current_position();
                src.set_position(start);
                return Err(Error::new(Span::new(start, end), "invalid token"))
            }

            return Ok(NumericLiteral::Number(i))

        } else {
            // all kinds of literal

            let mut i: i128;

            // first character must be integer
            if let Some(c) = src.match_char_range('0'..='9').await{
                i = (c as u32 - '0' as u32) as i128;
            } else{
                let end = src.current_position();
                src.set_position(start);
                return Err(Error::new(Span::new(start, end), "invalid token"))
            }

            // parse integer
            while let Some(c) = src.match_char_range('0'..='9').await{
                let n = (c as u32 - '0' as u32) as i128;
                i = i *10 + n;
            };
            
            // bigint
            if src.match_char('n').await{
                // return immediatly if bigint
                return Ok(NumericLiteral::BigInt(i))
            }

            // convert to float
            let mut re = i as f64;
            
            // parse the decimals
            if src.match_char('.').await{
                // the decimal value
                let mut de: f64 = 0.0;
                // digits
                let mut d: i32 = 0;
                while let Some(c) = src.match_char_range('0'..='9').await{
                    d += 1;
    
                    let n = (c as u32 - '0' as u32) as f64;
                    de += n / (10.0f64.powi(d));
                };

                // add decimals to result
                re += de;
            };

            // parse exponent
            if src.match_char('e').await || src.match_char('E').await{
                // positive by default
                let mut is_neg = false;

                let mut exp = 0;

                // negative or positive exponent
                if src.match_char('-').await{
                    is_neg = true;
                } else if src.match_char('+').await{
                    is_neg = false;
                };

                // exponent integer
                while let Some(c) = src.match_char_range('0'..='9').await{
                    let n = (c as u32 - '0' as u32) as i32;
                    exp = exp * 10 + n;
                };

                // negative exponent
                if is_neg{
                    exp = -exp;
                };

                // raise result to exponent
                re = re.powi(exp);
            }

            return Ok(NumericLiteral::Number(re))
        };
    }
}

#[derive(Debug, Default, Parse)]
#[grammar('"' $item0:(("\\\"" | !'"' ANY)*) '"')]
pub struct StringLiteral(pub Span);

#[derive(Debug, Default, Parse)]
#[grammar('`' $item0:(("\\`" | !'`' !"${" ANY | "${" TokenTree* "}")* ) '`')]
pub struct TemplateLiteral(pub Span);

#[derive(Debug, Default, Parse)]
#[grammar($tokens:TokenTree*)]
pub struct TokenStream{
    pub tokens: Vec<TokenTree>
}

pub enum Delimiter {
    Parenthesis,
    Brace,
    Bracket,
    None,
}

#[derive(Debug, Parse)]
pub enum TokenTree{
    #[grammar(WhiteSpaces $item0: IdentifierName WhiteSpaces)]
    Ident(IdentifierName),
    #[grammar(WhiteSpaces $item0: NumericLiteral WhiteSpaces)]
    LiteralNumberic(NumericLiteral),
    #[grammar(WhiteSpaces $item0: StringLiteral WhiteSpaces)]
    LiteralString(StringLiteral),
    #[grammar('(' $item0:Recursive<TokenTree>* ')')]
    GroupParanthessis(Vec<TokenTree>),
    #[grammar('{' $item0:Recursive<TokenTree>* '}')]
    GroupBrace(Vec<TokenTree>),
    #[grammar('[' $item0:Recursive<TokenTree>* ']')]
    GroupBracket(Vec<TokenTree>),
    #[grammar($item0:['`''!''£''$''%''^''&''*''-''+''='':''~''#''\'''<''>''?'',''.''/''|'] )]
    Punc(char),
}

impl Default for TokenTree{
    fn default() -> Self {
        Self::Ident(IdentifierName::default())
    }
}