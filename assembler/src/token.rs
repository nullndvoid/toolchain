use std::{num::ParseIntError, ops::Range, sync::Arc};

use dsa::common::prelude::*;
use logos::{Lexer, Logos};

/// A list of tokens.
pub struct TokenStream {
    /// A list of tokens -- the underlying Vec.
    stream: Vec<Token>,
}

impl TokenStream {
    pub fn new() -> Self { Self { stream: vec![] } }

    /// Appends a new [`Token`] to the [`TokenStream`].
    pub fn push(&mut self, token: Token) { self.stream.push(token); }
}

impl Default for TokenStream {
    fn default() -> Self { Self::new() }
}

impl IntoIterator for TokenStream {
    type Item = Token;
    type IntoIter = std::vec::IntoIter<Token>;

    fn into_iter(self) -> Self::IntoIter { self.stream.into_iter() }
}

/// Represents a [`Token`] in the source code.
#[derive(Debug, Clone)]
pub struct Token {
    /// The line number in the source code.
    line_number: usize,
    /// Where the token is in the line.
    line_span: Range<usize>,
    /// The type of [`Token`].
    token_type: TokenType,
}

impl Token {
    pub fn new(
        line_number: usize,
        line_span: Range<usize>,
        token_type: TokenType,
    ) -> Self {
        Self {
            line_number,
            line_span,
            token_type,
        }
    }

    pub fn line_span(&self) -> &Range<usize> { &self.line_span }

    pub fn token_type(&self) -> &TokenType { &self.token_type }

    pub fn line_number(&self) -> usize { self.line_number }
}

// (optional_label: instruction) | comment | directive

// directive = d(b|h|w) indent: vals (comma-separated) | "string"

// instruction = mnemonic arguments (comma-separated)

// argument = register | literal | symbol

// register = ... (defined registers)

// literal = 0x..., 0o..., 0..., [0-9]+

// symbol = anything otherwise (broadest regex)

#[derive(Debug, PartialEq, Logos, Clone)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(error = LexError)]
pub enum TokenType {
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]+:", label_callback)]
    /// TODO: Possibly merge this with the symbol token.
    Label(Arc<str>),
    #[regex(r"(db|dh|dw|resb|resh|resw)", priority = 5)]
    Directive,
    /// By far the messiest regex but it's simple to read and change as
    /// required.
    #[regex(
        r"(iadd|addi|isub|subi|push|nop|mov|movs|ldb|lb|ldbs|lbs)",
        priority = 5
    )]
    #[regex(
        r"(ldh|lh|ldhs|lhs|ldw|lw|stb|sb|sth|sh|stw|sw|lli|lui|jmp)",
        priority = 5
    )]
    #[regex(
        r"(jeq|jne|jgt|jge|jlt|jle|cmp|inc|dec|shl|shr|add|sub|and)",
        priority = 5
    )]
    #[regex(r"(or|not|xor|nand|nor|xnor|int|irt|hlt|jr|lhwmm|lidt)", priority = 5)]
    Instruction,
    #[regex(
        r"(rg([0-9]|1[0-5])|acc|spr|bpr|ret|idr|mmr|zero|noreg)",
        register_callback
    )]
    Register(Register),
    /// This can be trimmed smaller later but this should fit everything we
    /// need. I am aware this is a very broad pattern match, because we will
    /// later coerce the type to an address/literal as required.
    #[regex(r"0x([0-9a-fA-F_])+", integer_literal_callback)]
    #[regex(r"0o[0-7]+", integer_literal_callback)]
    #[regex(r"0b[0-1]+", integer_literal_callback)]
    #[regex(r"[0-9]+", integer_literal_callback)]
    IntegerLiteral(u32),
    #[regex(r#""[^"]*""#, string_callback, priority = 10)]
    String(Arc<str>),
    #[token(",")]
    Comma,
    #[token("(")]
    LeftBracket,
    #[token(")")]
    RightBracket,
    #[regex(r"\n")]
    Newline,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]+", symbol_callback)]
    Symbol(Arc<str>),
    #[regex(r"//.*\n", |_| logos::Skip)]
    LineComment,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub enum LexError {
    /// Errors arising from parsing integer literals.
    ParseInt(ParseIntError),
    #[default]
    /// For unknown errors returned by Logos.
    Other,
}

impl From<ParseIntError> for LexError {
    fn from(err: ParseIntError) -> Self { Self::ParseInt(err) }
}

/// Converts a token into an actual [`Register`].
fn register_callback(lex: &mut Lexer<TokenType>) -> Register {
    let token = lex.slice();

    match token {
        "rg0" => Register::Rg0,
        "rg1" => Register::Rg1,
        "rg2" => Register::Rg2,
        "rg3" => Register::Rg3,
        "rg4" => Register::Rg4,
        "rg5" => Register::Rg5,
        "rg6" => Register::Rg6,
        "rg7" => Register::Rg7,
        "rg8" => Register::Rg8,
        "rg9" => Register::Rg9,
        "rga" => Register::Rga,
        "rgb" => Register::Rgb,
        "rgc" => Register::Rgc,
        "rgd" => Register::Rgd,
        "rge" => Register::Rge,
        "rgf" => Register::Rgf,
        "acc" => Register::Acc,
        "spr" => Register::Spr,
        "bpr" => Register::Bpr,
        "ret" => Register::Ret,
        "idr" => Register::Idr,
        "mmr" => Register::Mmr,
        "zero" => Register::Zero,
        "noreg" => Register::NoReg,
        // "sts" => Register::Sts,
        _ => unreachable!(
            "All valid registers should be handled by the lexer. Something is odd."
        ),
    }
}

/// Converts an integer literal into a `u32`. This will fail if the
/// integer is too wide, so we need to handle this.
fn integer_literal_callback(lex: &mut Lexer<TokenType>) -> Result<u32, LexError> {
    Ok(parse_int::parse::<u32>(lex.slice())?)
}

/// Creates a string from
fn string_callback(lex: &mut Lexer<TokenType>) -> Arc<str> {
    let token_str = lex.slice();
    let token = token_str
        .strip_prefix("\"")
        .and_then(|t| t.strip_suffix("\""))
        .expect("Token should be wrapped in double quotes, panicking!");

    token.into()
}

fn label_callback(lex: &mut Lexer<TokenType>) -> Arc<str> {
    let token_str = lex.slice();
    let token = token_str
        .strip_suffix(":")
        .expect("Labels should probably have a suffix.");

    token.into()
}

fn symbol_callback(lex: &mut Lexer<TokenType>) -> Arc<str> { lex.slice().into() }
