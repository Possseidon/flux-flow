use std::num::NonZeroUsize;

use crate::compiler::typing::Type;

/// A list of elements with a potentially constrained length.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ListByLenType {
    element_type: Type,
    min_len: usize,
    max_len: NonZeroUsize,
}
