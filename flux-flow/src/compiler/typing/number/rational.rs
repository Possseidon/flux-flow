use std::sync::Arc;

use num::{BigRational, Signed, Zero};
use thiserror::Error;

/// A discrete or continuous range of rational numbers.
///
/// Unlike [`FloatType`], this type always represents exact values without doing any kind of
/// approximation or rounding during computations. This also means that is is a bit more limited
/// in functionality. E.g. trigonometry functions like `sin()` can only be approximated and thus
/// require the use of [`FloatType`].
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RationalRangeType {
    Discrete(DiscreteRationalRange),
    Continuous(ContinuousRationalRange),
}

impl RationalRangeType {
    /// Returns a value that can be used for sorting different ranges and [`BigRational`]s.
    pub fn sort_key(&self) -> Option<&Arc<BigRational>> {
        match self {
            Self::Discrete(range) => range.sort_key(),
            Self::Continuous(range) => range.sort_key(),
        }
    }

    pub fn contains(&self, value: &BigRational) -> bool {
        match self {
            Self::Discrete(range) => range.contains(value),
            Self::Continuous(range) => range.contains(value),
        }
    }
}

/// A discrete range of rational numbers.
///
/// Such a range always has a fixed step size, that must be greater than zero.
///
/// Additionally, bounds can be specified independently on both ends of the range. If neither a min
/// nor a max value is specified, the range is unbounded and contains an `offset_from_zero` value,
/// that can be used to shift the entire range.
///
/// `offset_from_zero` must be in the range `[0, step_size)`.
///
/// If either min or max is specified, that offset can be deduced from them. If both are specified,
/// max must be reachable using full steps.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct DiscreteRationalRange {
    /// The step size between each value in the range.
    ///
    /// Must be greater than zero.
    step: Arc<BigRational>,
    /// The optional bounds of the range.
    bounds: Discrete,
}

#[derive(Debug, Error)]
#[error("step size must be positive")]
pub struct StepNotPositive;

#[derive(Debug, Error)]
#[error("max must be reachable from min using full steps")]
pub struct UnreachableMaxValue;

#[derive(Debug, Error)]
#[error("offset from zero must be in the range [0, step_size)")]
pub struct InvalidOffsetFromZero;

#[derive(Debug, Error)]
#[error("min must be less than max")]
pub struct InvalidBound;

#[derive(Debug, Error)]
#[error(transparent)]
pub enum UnboundedWithOffsetError {
    StepIsZero(#[from] StepNotPositive),
    InvalidOffsetFromZero(#[from] InvalidOffsetFromZero),
}

#[derive(Debug, Error)]
#[error(transparent)]
pub enum BoundedError {
    StepIsZero(#[from] StepNotPositive),
    InvalidBound(#[from] InvalidBound),
    UnreachableMaxValue(#[from] UnreachableMaxValue),
}

impl DiscreteRationalRange {
    pub fn unbounded(step: Arc<BigRational>) -> Result<Self, StepNotPositive> {
        if step.is_zero() {
            return Err(StepNotPositive);
        }

        Ok(Self {
            step,
            bounds: Discrete::Unbounded {
                offset_from_zero: Arc::new(Zero::zero()),
            },
        })
    }

    pub fn unbounded_with_offset(
        offset_from_zero: Arc<BigRational>,
        step: Arc<BigRational>,
    ) -> Result<Self, UnboundedWithOffsetError> {
        if step.is_zero() {
            return Err(StepNotPositive.into());
        }

        if offset_from_zero >= step {
            return Err(InvalidOffsetFromZero.into());
        }

        Ok(Self {
            step,
            bounds: Discrete::Unbounded { offset_from_zero },
        })
    }

    pub fn bounded(
        min_value: Arc<BigRational>,
        max_value: Arc<BigRational>,
        step: Arc<BigRational>,
    ) -> Result<Self, BoundedError> {
        if !step.is_positive() {
            return Err(StepNotPositive.into());
        }

        if min_value >= max_value {
            return Err(InvalidBound.into());
        }

        let diff = max_value.as_ref() - min_value.as_ref();

        if !(diff % step.as_ref()).is_zero() {
            return Err(UnreachableMaxValue.into());
        }

        Ok(Self {
            step,
            bounds: Discrete::Between {
                min_value,
                max_value,
            },
        })
    }

    pub fn at_least(
        min_value: Arc<BigRational>,
        step: Arc<BigRational>,
    ) -> Result<Self, StepNotPositive> {
        if step.is_zero() {
            return Err(StepNotPositive);
        }

        Ok(Self {
            step,
            bounds: Discrete::AtLeas(min_value),
        })
    }

    pub fn at_most(
        max_value: Arc<BigRational>,
        step: Arc<BigRational>,
    ) -> Result<Self, StepNotPositive> {
        if step.is_zero() {
            return Err(StepNotPositive);
        }

        Ok(Self {
            step,
            bounds: Discrete::AtMost(max_value),
        })
    }

    pub fn sort_key(&self) -> Option<&Arc<BigRational>> {
        match &self.bounds {
            Discrete::Unbounded { .. } => None,
            Discrete::AtLeas(min_value) => Some(min_value),
            Discrete::AtMost(_) => None,
            Discrete::Between { min_value, .. } => Some(min_value),
        }
    }

    pub fn contains(&self, value: &BigRational) -> bool {
        match &self.bounds {
            Discrete::Unbounded { offset_from_zero } => {
                ((value - offset_from_zero.as_ref()) % self.step.as_ref()).is_zero()
            }
            Discrete::AtLeas(min_value) => {
                value >= min_value.as_ref()
                    && ((value - min_value.as_ref()) % self.step.as_ref()).is_zero()
            }
            Discrete::AtMost(max_value) => {
                value <= max_value.as_ref()
                    && ((value - max_value.as_ref()) % self.step.as_ref()).is_zero()
            }
            Discrete::Between {
                min_value,
                max_value,
            } => {
                value >= min_value.as_ref()
                    && value <= max_value.as_ref()
                    && ((value - min_value.as_ref()) % self.step.as_ref()).is_zero()
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Discrete {
    Unbounded {
        offset_from_zero: Arc<BigRational>,
    },
    AtLeas(Arc<BigRational>),
    AtMost(Arc<BigRational>),
    Between {
        min_value: Arc<BigRational>,
        max_value: Arc<BigRational>,
    },
}

/// A continuous range of rational numbers with optional inclusive/exclusive bounds.
///
/// `min_value` must be less than `max_value`.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ContinuousRationalRange {
    min_value: Option<ContinuousRationalBound>,
    max_value: Option<ContinuousRationalBound>,
}

impl ContinuousRationalRange {
    pub fn new(
        min_value: Option<ContinuousRationalBound>,
        max_value: Option<ContinuousRationalBound>,
    ) -> Option<Self> {
        if let (Some(min_value), Some(max_value)) = (&min_value, &max_value) {
            if min_value >= max_value {
                return None;
            }
        }
        Some(Self {
            min_value,
            max_value,
        })
    }

    fn sort_key(&self) -> Option<&Arc<BigRational>> {
        self.min_value.as_ref().map(|min_value| match min_value {
            ContinuousRationalBound::Inclusive(value)
            | ContinuousRationalBound::Exclusive(value) => value,
        })
    }

    fn contains(&self, value: &BigRational) -> bool {
        if let Some(min_value) = &self.min_value {
            match min_value {
                ContinuousRationalBound::Inclusive(min_value) => {
                    if value < min_value {
                        return false;
                    }
                }
                ContinuousRationalBound::Exclusive(min_value) => {
                    if value <= min_value {
                        return false;
                    }
                }
            }
        }

        if let Some(max_value) = &self.max_value {
            match max_value {
                ContinuousRationalBound::Inclusive(max_value) => {
                    if value > max_value {
                        return false;
                    }
                }
                ContinuousRationalBound::Exclusive(max_value) => {
                    if value >= max_value {
                        return false;
                    }
                }
            }
        }

        true
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ContinuousRationalBound {
    Inclusive(Arc<BigRational>),
    Exclusive(Arc<BigRational>),
}
