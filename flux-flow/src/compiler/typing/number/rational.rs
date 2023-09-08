use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use num::BigRational;

use super::unit::NumberUnit;

/// A collection of number types, identified by their [`NumberUnit`].
#[derive(Default)]
pub(crate) struct RationalType {
    ranges: Option<Arc<BTreeMap<NumberUnit, BTreeSet<RationalRangeType>>>>,
}

/// A discrete or continuous range of rational numbers.
///
/// Unlike [`FloatType`], this type always represents exact values without doing any kind of
/// approximation or rounding during computations. This also means that is is a bit more limited
/// in functionality. E.g. trigonometry functions like `sin()` can only be approximated and thus
/// require the use of [`FloatType`].
enum RationalRangeType {
    Discrete(DiscreteRationalRange),
    Continuous(ContinuousRationalRange),
}

/// A discrete range of rational numbers.
///
/// Such a range always has a fixed step size, that can be negative but never zero.
///
/// Additionally, bounds can be specified independently on both ends of the range. If neither a min
/// nor a max value is specified, the range is unbounded and contains an `offset_from_zero` value,
/// that can be used to shift the entire range.
///
/// If either min or max is specified, that offset can be deduced from them. If both are specified,
/// max must be reachable using full steps.
struct DiscreteRationalRange {
    /// The step size between each value in the range.
    ///
    /// Can be positive or negative, but never zero.
    ///
    /// Additionally, a positive `step` implies `start` < `stop` and vice versa.
    step: Arc<BigRational>,
    /// The optional bounds of the range.
    bounds: DiscreteRationalRangeBounds,
}

enum DiscreteRationalRangeBounds {
    Unbounded {
        offset_from_zero: Arc<BigRational>,
    },
    Min(Arc<BigRational>),
    Max(Arc<BigRational>),
    Both {
        start: Arc<BigRational>,
        stop: Arc<BigRational>,
    },
}

/// A continuous range of rational numbers with optional inclusive/exclusive bounds.
///
/// `min` must not be greater than `max`, but an empty range is ok but has no value: `0 < .. < 0`
struct ContinuousRationalRange {
    min: Option<ContinuousRationalBound>,
    max: Option<ContinuousRationalBound>,
}

enum ContinuousRationalBound {
    Inclusive(Arc<BigRational>),
    Exclusive(Arc<BigRational>),
}
