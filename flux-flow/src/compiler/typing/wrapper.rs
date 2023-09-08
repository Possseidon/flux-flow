use std::sync::Arc;

/// A type that can represent set of wrapped different types.
///
/// Unlike with most other types, the order can be freely specified by the user and is, in fact, the
/// only way to specify custom orders between types.
///
/// Does not allow duplicates for obvious reasons.
#[derive(Default)]
pub(crate) struct WrappedTypes {
    types: Option<Arc<[WrappedTypeRef]>>,
}

/// Index into a list of wrapper types.
struct WrappedTypeRef {
    index: usize,
}
