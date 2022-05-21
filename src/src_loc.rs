use std::fmt;

/// An internal source code location
#[derive(Clone, Debug)]
pub struct SrcLoc {
    /// character offset into source
    pub offset: usize,
    /// length of the lexeme
    pub length: usize,
}

impl SrcLoc {
    /// Construct a dummy source location; used only in testing
    pub fn dummy() -> Self {
        SrcLoc {
            offset: 0,
            length: 0,
        }
    }
}

impl fmt::Display for SrcLoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "chars({},{})", self.offset, self.offset + self.length)
    }
}
