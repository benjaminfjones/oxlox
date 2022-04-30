use std::char;

#[derive(Debug)]
pub struct Token {
    pub source: String,
}

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
            .map(|s| Ok(Token { source: s.to_string()}))
            .collect()
    }
}
