use std::sync::Arc;

use super::Type;

/// Types can be stored as values and a meta-type describes the subset of types that can be stored.
#[derive(Default)]
pub(crate) struct MetaTypes {
    ty: Option<Arc<Type>>,
}
