use std::{collections::BTreeMap, num::NonZeroUsize, sync::Arc};

use crate::compiler::typing::{Type, Value};

/// A type that can represent a map of distinct keys to values.
///
/// The empty map shall be represented by [`MapType::Empty`].
#[derive(Default)]
pub(crate) enum MapType {
    #[default]
    Never,
    Empty,
    ByValues(MapTypeByValues),
    ByLen(MapTypeByLen),
}

/// A map that can only represent a fixed set of key-value pairs.
///
/// Its key and value types can be deduced from its entries.
pub(crate) struct MapTypeByValues {
    values: Arc<BTreeMap<Value, Value>>,
}

/// A map of key-value pairs with a potentially constrained length.
pub(crate) struct MapTypeByLen {
    key_type: Arc<Type>,
    value_type: Arc<Type>,
    min_len: usize,
    max_len: NonZeroUsize,
}
