use std::char;

use crate::token::{Token, TokenType};
use crate::src_loc::SrcLoc;

pub struct Scanner {
    source: String,
}

#[derive(Debug)]
pub struct ScanError {
    pub message: String,
}

impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner { source }
    }

    /// Scan the source, returning a list of tokens and/or errors
    pub fn scan(self) -> Vec<Result<Token, ScanError>> {
        self.source.split(char::is_whitespace)
            .map(|s| Ok(Token::new(
                        TokenType::String,
                        s.to_string(),
                        None,
                        SrcLoc {offset: 0})))
            .collect()
    }
}
