use std::num::NonZeroUsize;

use crate::compiler::typing::Type;

/// A set of elements with a potentially constrained length.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct SetByLenType {
    element_type: Type,
    min_len: usize,
    max_len: NonZeroUsize,
}
