use std::num::NonZeroUsize;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct CharRangeType {
    min_char: char,
    max_char: char,
}

impl CharRangeType {
    pub fn new(min_char: char, max_char: char) -> Option<Self> {
        (min_char <= max_char).then_some(Self { min_char, max_char })
    }

    pub fn min_char(self) -> char {
        self.min_char
    }

    pub fn max_char(self) -> char {
        self.max_char
    }
}

/// A string with a potentially constrained length and set of characters.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StringByLenType {
    min_len: usize,
    max_len: NonZeroUsize,
}
