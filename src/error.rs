use crate::{src_loc::SrcLoc, token::Token};
/// Common type and implementation for parser/scanner/runtime errors
use std::fmt;

#[derive(Debug, PartialEq, Eq)]
pub enum ErrorType {
    ScanError,
    ParseError,
    RuntimeError,
}

#[derive(Debug)]
pub struct BaseError {
    error_type: ErrorType,
    message: String,
    src_loc: Option<SrcLoc>,
    token: Option<Token>,
}

impl BaseError {
    pub fn new(error_type: ErrorType, message: &str) -> Self {
        BaseError {
            error_type,
            message: message.to_string(),
            src_loc: None,
            token: None,
        }
    }

    pub fn with_token(self, token: Token) -> Self {
        // TODO: share or consolidate source location data
        let src_loc_copy = token.src_loc.clone();
        BaseError {
            token: Some(token),
            src_loc: Some(src_loc_copy),
            ..self
        }
    }

    pub fn with_src_loc(self, src_loc: SrcLoc) -> Self {
        BaseError {
            src_loc: Some(src_loc),
            ..self
        }
    }

    /// Helper function for constructing common runtime errors with a message and token
    /// representing source locattion.
    pub fn runtime_error(msg: &str, token: &Token) -> Self {
        Self::new(ErrorType::RuntimeError, msg).with_token(token.to_owned())
    }

    pub fn error_type(&self) -> &ErrorType {
        &self.error_type
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    // TODO: if there is a token, look in the token for src_loc as well?
    pub fn src_loc(&self) -> Option<&SrcLoc> {
        self.src_loc.as_ref()
    }
}

// TODO: render errors with original source program text
impl fmt::Display for BaseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let src_loc_str = if let Some(s) = self.src_loc.as_ref() {
            format!("{}", s)
        } else if let Some(t) = self.token.as_ref() {
            format!("{}", t.src_loc)
        } else {
            "unknown location".to_string()
        };
        write!(f, "{:?}:{}:{}", self.error_type, src_loc_str, &self.message)
    }
}

impl From<BaseError> for String {
    fn from(e: BaseError) -> Self {
        format!("{}", e)
    }
}

/// A collection of Oxlox Errors
#[derive(Debug, Default)]
pub struct ErrorList {
    errors: Vec<BaseError>,
}

impl ErrorList {
    pub fn add(&mut self, error: BaseError) {
        self.errors.push(error);
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
}

impl IntoIterator for ErrorList {
    type Item = BaseError;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.into_iter()
    }
}
