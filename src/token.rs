use crate::src_loc::SrcLoc;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
  // Single-character tokens
  LeftParen, RightParen, LeftBrace, RightBrace,
  Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

  // One or two character tokens
  Bang, BangEqual,
  Equal, EqualEqual,
  Greater, GreaterEqual,
  Less, LessEqual,

  // Literals
  Identifier, String, Number,

  // Keywords
  And, Class, Else, False, Fun, For, If, Nil, Or,
  Print, Return, Super, This, True, Var, While,

  Eof
}

#[derive(Debug)]
pub enum TokenLiteral {
    Identifier(String),
    String(String),
    Nunmber(f64),
}

/// Tokens
///
/// TODO: represent source using string slices into scanner source
#[derive(Debug)]
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
    pub fn new(typ: TokenType, lexeme: Option<String>, literal: Option<TokenLiteral>, src_loc: SrcLoc) -> Self {
        Token {typ, lexeme, literal, src_loc}
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
