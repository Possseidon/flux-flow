use std::{
    collections::BTreeSet,
    ops::{BitAnd, BitOr, BitXor, Sub},
};

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct UnitType {
    /// The empty set **must** be represented as [`None`].
    units: BTreeSet<NumberUnit>,
}

impl UnitType {
    pub(crate) fn is_never(&self) -> bool {
        self.units.is_empty()
    }
}

impl BitOr for &UnitType {
    type Output = UnitType;

    fn bitor(self, rhs: Self) -> Self::Output {
        UnitType {
            units: &self.units | &rhs.units,
        }
    }
}

impl BitAnd for &UnitType {
    type Output = UnitType;

    fn bitand(self, rhs: Self) -> Self::Output {
        UnitType {
            units: &self.units & &rhs.units,
        }
    }
}

impl BitXor for &UnitType {
    type Output = UnitType;

    fn bitxor(self, rhs: Self) -> Self::Output {
        UnitType {
            units: &self.units ^ &rhs.units,
        }
    }
}
impl Sub for &UnitType {
    type Output = UnitType;

    fn sub(self, rhs: Self) -> Self::Output {
        UnitType {
            units: &self.units - &rhs.units,
        }
    }
}

/// The unit of a number.
///
/// While there are a lot of built-in ones, it is possible and often recommended to define your own.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct NumberUnit;
