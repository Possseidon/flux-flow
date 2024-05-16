use std::cmp::Ordering;
use std::{borrow::Cow, sync::Arc};

use std::num::NonZeroU128;

use malachite::Rational;

#[derive(Clone, Debug, Eq)]
pub(super) enum NumberImpl {
    PositiveSmall(u128),
    NegativeSmall(NonZeroU128),
    Rational(Arc<Rational>),
}

impl NumberImpl {
    fn into_rational(self) -> Rational {
        match self {
            Self::PositiveSmall(num) => Rational::from(num),
            Self::NegativeSmall(num) => -Rational::from(num.get()),
            Self::Rational(num) => Arc::unwrap_or_clone(num),
        }
    }

    fn to_rational(&self) -> Cow<Rational> {
        match self {
            Self::PositiveSmall(num) => Cow::Owned(Rational::from(*num)),
            Self::NegativeSmall(num) => Cow::Owned(-Rational::from(num.get())),
            Self::Rational(num) => Cow::Borrowed(num),
        }
    }
}

impl Default for NumberImpl {
    fn default() -> Self {
        Self::PositiveSmall(0)
    }
}

impl std::hash::Hash for NumberImpl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.to_rational().hash(state)
    }
}

impl PartialEq for NumberImpl {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl PartialOrd for NumberImpl {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NumberImpl {
    fn cmp(&self, other: &Self) -> Ordering {
        self.to_rational().cmp(&other.to_rational())
    }
}
