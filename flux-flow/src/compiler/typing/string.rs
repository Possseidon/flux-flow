use std::collections::BTreeSet;

use std::sync::Arc;

/// The type of a single character.
#[derive(Default)]
pub(crate) struct CharType {
    ranges: Option<Arc<BTreeSet<CharRangeType>>>,
}

pub(crate) struct CharRangeType {
    pub(crate) min: char,
    pub(crate) max: char,
}

/// A type of a string limited either by its length or by a fixed set of values.
///
/// The empty string can technically be represented by both variants, but since [`StringTypeByLen`]
/// does not require allocations, it shall be used for this purpose.
#[derive(Default)]
pub(crate) enum StringType {
    #[default]
    Never,
    Values(StringTypeByValues),
    Sized(StringTypeByLen),
}

/// A string that can only represent a fixed set of values.
pub(crate) struct StringTypeByValues {
    pub(crate) values: Arc<BTreeSet<Arc<str>>>,
}

/// A string with a potentially constrained length.
pub(crate) struct StringTypeByLen {
    pub(crate) min_len: usize,
    pub(crate) max_len: usize,
}
