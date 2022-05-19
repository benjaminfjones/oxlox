use std::fmt;
use crate::{src_loc::SrcLoc, ptypes::PInt};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

/// Lookup Keywords
/// TODO: keyword lookup: try using lazy_static + HashMap
pub fn lookup_keyword(val: &str) -> Option<TokenType> {
    match val {
        "and" => Some(TokenType::And),
        "class" => Some(TokenType::Class),
        "else" => Some(TokenType::Else),
        "false" => Some(TokenType::False),
        "fun" => Some(TokenType::Fun),
        "for" => Some(TokenType::For),
        "if" => Some(TokenType::If),
        "nil" => Some(TokenType::Nil),
        "or" => Some(TokenType::Or),
        "print" => Some(TokenType::Print),
        "return" => Some(TokenType::Return),
        "super" => Some(TokenType::Super),
        "this" => Some(TokenType::This),
        "true" => Some(TokenType::True),
        "var" => Some(TokenType::Var),
        "while" => Some(TokenType::While),
        _ => None,
    }
}

#[derive(Clone, Debug)]
pub enum TokenLiteral {
    Identifier(String),
    String(String),
    Number(PInt),
}

/// Tokens
///
/// TODO: represent source using string slices into scanner source
#[derive(Clone, Debug)]
pub struct Token {
    /// Type of the token
    pub typ: TokenType,
    /// Original source text of the token; basic tokens store None
    pub lexeme: Option<String>,
    /// Runtime literal for the token; only stored for Literals
    pub literal: Option<TokenLiteral>,
    /// Source location of the token
    pub src_loc: SrcLoc,
}

impl Token {
    pub fn new(
        typ: TokenType,
        lexeme: Option<String>,
        literal: Option<TokenLiteral>,
        src_loc: SrcLoc,
    ) -> Self {
        Token {
            typ,
            lexeme,
            literal,
            src_loc,
        }
    }

    /// Return an end-of-file token at the given offset
    pub fn eof(offset: usize) -> Token {
        Token {
            typ: TokenType::Eof,
            lexeme: None,
            literal: None,
            src_loc: SrcLoc { offset, length: 0 },
        }
    }

    pub fn is_eof(&self) -> bool {
        self.typ == TokenType::Eof
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(lexeme) = self.lexeme.as_ref() {
            write!(f, "{}", lexeme)
        } else {
            write!(f, "{:?}", self.typ)
        }
    }
}
