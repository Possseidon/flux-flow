use std::ops::{BitAnd, BitOr, BitXor, Neg, Sub};

/// The type of a boolean, including not only `true` and `false` but also `maybe`.
///
/// Can represent `never` when all flags are `false`.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct BooleanType {
    /// Whether the type can hold the `true` value.
    pub(crate) can_true: bool,
    /// Whether the type can hold the `false` value.
    pub(crate) can_false: bool,
    /// Whether the type can hold the `maybe` value.
    pub(crate) can_maybe: bool,
}

impl BooleanType {
    pub(crate) fn is_never(self) -> bool {
        self == Default::default()
    }
}

impl Neg for &BooleanType {
    type Output = BooleanType;

    fn neg(self) -> Self::Output {
        BooleanType {
            can_true: !self.can_true,
            can_false: !self.can_false,
            can_maybe: !self.can_maybe,
        }
    }
}

impl BitOr for &BooleanType {
    type Output = BooleanType;

    fn bitor(self, rhs: Self) -> Self::Output {
        BooleanType {
            can_true: self.can_true | rhs.can_true,
            can_false: self.can_false | rhs.can_false,
            can_maybe: self.can_maybe | rhs.can_maybe,
        }
    }
}

impl BitAnd for &BooleanType {
    type Output = BooleanType;

    fn bitand(self, rhs: Self) -> Self::Output {
        BooleanType {
            can_true: self.can_true & rhs.can_true,
            can_false: self.can_false & rhs.can_false,
            can_maybe: self.can_maybe & rhs.can_maybe,
        }
    }
}

impl BitXor for &BooleanType {
    type Output = BooleanType;

    fn bitxor(self, rhs: Self) -> Self::Output {
        BooleanType {
            can_true: self.can_true ^ rhs.can_true,
            can_false: self.can_false ^ rhs.can_false,
            can_maybe: self.can_maybe ^ rhs.can_maybe,
        }
    }
}

impl Sub for &BooleanType {
    type Output = BooleanType;

    fn sub(self, rhs: Self) -> Self::Output {
        self & -rhs
    }
}
