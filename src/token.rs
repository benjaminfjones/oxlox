use crate::src_loc::SrcLoc;

#[derive(Debug)]
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
    pub typ: TokenType,
    pub lexeme: String,
    pub literal: Option<TokenLiteral>,
    pub src_loc: SrcLoc,
}

impl Token {
    pub fn new(typ: TokenType, lexeme: String, literal: Option<TokenLiteral>, src_loc: SrcLoc) -> Self {
        Token {typ, lexeme, literal, src_loc}
    }
}
