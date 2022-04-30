/// An internal source code location
#[derive(Debug)]
pub struct SrcLoc {
    /// character offset into source
    pub offset: usize,
    /// length of the lexeme
    pub length: usize,
}
