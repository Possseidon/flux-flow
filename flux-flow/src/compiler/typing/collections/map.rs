use std::num::NonZeroUsize;

use crate::compiler::typing::Type;

/// A map of key-value pairs with a potentially constrained length.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct MapByLenType {
    key_type: Type,
    value_type: Type,
    min_len: usize,
    max_len: NonZeroUsize,
}
