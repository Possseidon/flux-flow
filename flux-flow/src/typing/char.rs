#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CharType {
    Char(char),
    CharRange(CharRange),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CharRange {
    min: char,
    max: char,
}
