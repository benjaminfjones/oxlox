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
