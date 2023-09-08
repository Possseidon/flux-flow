use std::{collections::BTreeSet, sync::Arc};

use ordered_float::NotNan;

#[derive(Default)]
pub(crate) struct FloatType {
    ranges: Option<Arc<BTreeSet<FloatRangeType>>>,
    /// Whether `NaN` can be represented.
    supports_nan: bool,
}

/// The type of a floating point number that tries to approximate real numbers.
struct FloatRangeType {
    /// The minimum number this type can store (inclusive).
    min: NotNan<f64>,
    /// The maximum number this type can store (inclusive).
    max: NotNan<f64>,
}
