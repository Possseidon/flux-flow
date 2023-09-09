use std::{
    cmp::Ordering,
    fmt::Display,
    ops::{BitAnd, BitOr, BitXor, Sub},
};

use num::Float;
use ordered_float::{FloatIsNan, NotNan};

/// The type of a floating point number that tries to approximate real numbers.
///
/// Bounds are always inclusive, but when formatting, it can switch between inclusive and
/// exclusive bounds, depending on which string representation is more compact.
#[derive(Clone, Copy, Debug)]
pub struct FloatRangeType<F: Float> {
    /// The minimum number this type can store (inclusive).
    ///
    /// Must not be greater than `max_value`.
    min_value: NotNan<F>,
    /// The maximum number this type can store (inclusive).
    max_value: NotNan<F>,
}

impl<F: Float> PartialEq for FloatRangeType<F> {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl<F: Float> Eq for FloatRangeType<F> {}

impl<F: Float> PartialOrd for FloatRangeType<F> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<F: Float> Ord for FloatRangeType<F> {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.min_value, self.max_value).cmp(&(other.min_value, other.max_value))
    }
}

impl<F: Float> FloatRangeType<F> {
    /// Creates a new range.
    ///
    /// Returns [`None`] if `min_value` is greater than `max_value`.
    pub fn new(min_value: NotNan<F>, max_value: NotNan<F>) -> Option<Self> {
        (min_value <= max_value).then_some(Self {
            min_value,
            max_value,
        })
    }

    pub fn min_value(self) -> NotNan<F> {
        self.min_value
    }

    pub fn max_value(self) -> NotNan<F> {
        self.max_value
    }

    pub fn contains(&self, value: NotNan<F>) -> bool {
        self.min_value <= value && value <= self.max_value
    }

    pub fn display(self) -> RangeDisplay
    where
        F: Display,
        NotNan<F>: FloatStep,
    {
        if self.min_value == self.max_value {
            RangeDisplay::Value(self.min_value.to_string())
        } else {
            RangeDisplay::Range {
                min_value: self.min_display(),
                max_value: self.max_display(),
            }
        }
    }

    pub fn min_display(self) -> BoundDisplay
    where
        F: Display,
        NotNan<F>: FloatStep,
    {
        let exact = self.min_value.to_string();
        let prev = self.min_value.next_down().to_string();

        if exact.len() < prev.len() {
            BoundDisplay::LessThanOrEqual(exact)
        } else {
            BoundDisplay::LessThan(prev)
        }
    }

    pub fn max_display(self) -> BoundDisplay
    where
        F: Display,
        NotNan<F>: FloatStep,
    {
        let exact = self.max_value.to_string();
        let next = self.max_value.next_up().to_string();

        if exact.len() < next.len() {
            BoundDisplay::LessThanOrEqual(exact)
        } else {
            BoundDisplay::LessThan(next)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RangeDisplay {
    Value(String),
    Range {
        min_value: BoundDisplay,
        max_value: BoundDisplay,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoundDisplay {
    LessThanOrEqual(String),
    LessThan(String),
}

impl<F: Float> BitOr for FloatRangeType<F>
where
    NotNan<F>: FloatStep,
{
    type Output = FloatRangeBitOr<F>;

    fn bitor(self, rhs: Self) -> Self::Output {
        let (lower_min, higher_min) = match self.cmp(&rhs) {
            Ordering::Less => (self, rhs),
            Ordering::Equal => return FloatRangeBitOr::One(self),
            Ordering::Greater => (rhs, self),
        };

        if lower_min.max_value.has_gap(higher_min.min_value) {
            // cannot merge because of gap
            FloatRangeBitOr::Two {
                lower: lower_min,
                higher: higher_min,
            }
        } else {
            FloatRangeBitOr::One(Self {
                // we already know, that lower_min is lower
                min_value: lower_min.min_value,
                // but we don't know yet which max_value is higher
                max_value: lower_min.max_value.max(higher_min.max_value),
            })
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatRangeBitOr<F: Float> {
    One(FloatRangeType<F>),
    Two {
        lower: FloatRangeType<F>,
        higher: FloatRangeType<F>,
    },
}

impl<F: Float> BitAnd for FloatRangeType<F> {
    type Output = FloatRangeBitAnd<F>;

    fn bitand(self, rhs: Self) -> Self::Output {
        let (lower_min, higher_min) = match self.cmp(&rhs) {
            Ordering::Less => (self, rhs),
            Ordering::Equal => return FloatRangeBitAnd::One(self),
            Ordering::Greater => (rhs, self),
        };

        if lower_min.max_value < higher_min.min_value {
            FloatRangeBitAnd::None
        } else {
            FloatRangeBitAnd::One(Self {
                min_value: higher_min.min_value,
                max_value: lower_min.max_value.min(higher_min.max_value),
            })
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatRangeBitAnd<F: Float> {
    None,
    One(FloatRangeType<F>),
}

impl<F: Float> BitXor for FloatRangeType<F>
where
    NotNan<F>: FloatStep,
{
    type Output = FloatRangeBitXor<F>;

    fn bitxor(self, rhs: Self) -> Self::Output {
        let (lower_min, higher_min) = match self.cmp(&rhs) {
            Ordering::Less => (self, rhs),
            Ordering::Equal => return FloatRangeBitXor::None,
            Ordering::Greater => (rhs, self),
        };

        if lower_min.max_value.has_gap(higher_min.min_value) {
            // cannot merge because of gap
            FloatRangeBitXor::Two {
                lower: lower_min,
                higher: higher_min,
            }
        } else if lower_min.max_value.next_up() == higher_min.min_value {
            // touching
            FloatRangeBitXor::One(Self {
                min_value: lower_min.min_value,
                max_value: higher_min.max_value,
            })
        } else if lower_min.min_value == higher_min.min_value {
            // overlap with same starting point
            FloatRangeBitXor::One(Self {
                min_value: lower_min.max_value.min(higher_min.max_value).next_up(),
                max_value: lower_min.max_value.max(higher_min.max_value),
            })
        } else if lower_min.max_value == higher_min.max_value {
            // overlap with same ending point
            FloatRangeBitXor::One(Self {
                min_value: lower_min.min_value,
                max_value: higher_min.min_value.next_down(),
            })
        } else if higher_min.max_value < lower_min.max_value {
            // enclosed
            FloatRangeBitXor::Two {
                lower: Self {
                    min_value: lower_min.min_value,
                    max_value: higher_min.min_value.next_down(),
                },
                higher: Self {
                    min_value: higher_min.max_value.next_up(),
                    max_value: lower_min.max_value,
                },
            }
        } else {
            // overlap
            FloatRangeBitXor::Two {
                lower: Self {
                    min_value: lower_min.min_value,
                    max_value: higher_min.min_value.next_down(),
                },
                higher: Self {
                    min_value: lower_min.max_value.next_up(),
                    max_value: higher_min.max_value,
                },
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatRangeBitXor<F: Float> {
    None,
    One(FloatRangeType<F>),
    Two {
        lower: FloatRangeType<F>,
        higher: FloatRangeType<F>,
    },
}

impl<F: Float> Sub for FloatRangeType<F>
where
    NotNan<F>: FloatStep,
{
    type Output = FloatRangeSub<F>;

    fn sub(self, rhs: Self) -> Self::Output {
        if self.min_value > rhs.max_value || self.max_value < rhs.min_value {
            return FloatRangeSub::One(self);
        }
        match (
            self.min_value.cmp(&rhs.min_value),
            self.max_value.cmp(&rhs.max_value),
        ) {
            (Ordering::Less, Ordering::Greater) => FloatRangeSub::Two {
                lower: FloatRangeType {
                    min_value: self.min_value,
                    max_value: rhs.min_value.next_down(),
                },
                higher: FloatRangeType {
                    min_value: rhs.max_value.next_up(),
                    max_value: self.max_value,
                },
            },
            (Ordering::Less, Ordering::Less | Ordering::Equal) => {
                FloatRangeSub::One(FloatRangeType {
                    min_value: self.min_value,
                    max_value: rhs.min_value.next_down(),
                })
            }
            (Ordering::Equal | Ordering::Greater, Ordering::Greater) => {
                FloatRangeSub::One(FloatRangeType {
                    min_value: rhs.max_value.next_up(),
                    max_value: self.max_value,
                })
            }
            (Ordering::Equal | Ordering::Greater, Ordering::Less | Ordering::Equal) => {
                FloatRangeSub::None
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatRangeSub<F: Float> {
    None,
    One(FloatRangeType<F>),
    Two {
        lower: FloatRangeType<F>,
        higher: FloatRangeType<F>,
    },
}

pub trait FloatStep {
    fn next_up(self) -> Self;
    fn next_down(self) -> Self;

    /// Whether there is at least one representable value between self and other (assuming self < other).
    fn has_gap(self, other: Self) -> bool
    where
        Self: Sized + Ord,
    {
        self.next_up() < other
    }
}

impl FloatStep for NotNan<f64> {
    fn next_up(self) -> Self {
        const TINY_BITS: u64 = 0x1;
        const CLEAR_SIGN_MASK: u64 = 0x7fff_ffff_ffff_ffff;

        let bits = self.to_bits();
        if bits == f64::INFINITY.to_bits() {
            return self;
        }

        let abs = bits & CLEAR_SIGN_MASK;
        let next_bits = if abs == 0 {
            TINY_BITS
        } else if bits == abs {
            bits + 1
        } else {
            bits - 1
        };

        match NotNan::new(f64::from_bits(next_bits)) {
            Ok(value) => value,
            Err(FloatIsNan) => unreachable!("float should not be NaN"),
        }
    }

    fn next_down(self) -> Self {
        const NEG_TINY_BITS: u64 = 0x8000_0000_0000_0001;
        const CLEAR_SIGN_MASK: u64 = 0x7fff_ffff_ffff_ffff;

        let bits = self.to_bits();
        if bits == f64::NEG_INFINITY.to_bits() {
            return self;
        }

        let abs = bits & CLEAR_SIGN_MASK;
        let next_bits = if abs == 0 {
            NEG_TINY_BITS
        } else if bits == abs {
            bits - 1
        } else {
            bits + 1
        };

        match NotNan::new(f64::from_bits(next_bits)) {
            Ok(value) => value,
            Err(FloatIsNan) => unreachable!("float should not be NaN"),
        }
    }
}

impl FloatStep for NotNan<f32> {
    fn next_up(self) -> Self {
        const TINY_BITS: u32 = 0x1;
        const CLEAR_SIGN_MASK: u32 = 0x7fff_ffff;

        let bits = self.to_bits();
        if bits == f32::INFINITY.to_bits() {
            return self;
        }

        let abs = bits & CLEAR_SIGN_MASK;
        let next_bits = if abs == 0 {
            TINY_BITS
        } else if bits == abs {
            bits + 1
        } else {
            bits - 1
        };

        match Self::new(f32::from_bits(next_bits)) {
            Ok(value) => value,
            Err(FloatIsNan) => unreachable!("float should not be NaN"),
        }
    }

    fn next_down(self) -> Self {
        const NEG_TINY_BITS: u32 = 0x8000_0001;
        const CLEAR_SIGN_MASK: u32 = 0x7fff_ffff;

        let bits = self.to_bits();
        if bits == f32::NEG_INFINITY.to_bits() {
            return self;
        }

        let abs = bits & CLEAR_SIGN_MASK;
        let next_bits = if abs == 0 {
            NEG_TINY_BITS
        } else if bits == abs {
            bits - 1
        } else {
            bits + 1
        };

        match Self::new(f32::from_bits(next_bits)) {
            Ok(value) => value,
            Err(FloatIsNan) => unreachable!("float should not be NaN"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use super::*;

    use enum_map::{enum_map, Enum, EnumMap};
    use num::{One, Zero};

    #[test]
    fn test_new_float_range_type() {
        let one = NotNan::new(1.0).unwrap();
        let two = NotNan::new(2.0).unwrap();

        let range = FloatRangeType::new(one, two);
        assert_eq!(
            range,
            Some(FloatRangeType {
                min_value: one,
                max_value: two
            })
        );

        let range = FloatRangeType::new(one, one);
        assert_eq!(
            range,
            Some(FloatRangeType {
                min_value: one,
                max_value: one
            })
        );

        let range = FloatRangeType::new(two, one);
        assert_eq!(range, None);
    }

    #[test]
    fn test_display_float_range_type() {
        let one = NotNan::new(1.0).unwrap();
        let two = NotNan::new(2.0).unwrap();

        // 1
        let display = FloatRangeType::new(one, one).unwrap().display();
        assert_eq!(display, RangeDisplay::Value(one.to_string()));

        // 1 <= .. <= 2
        let display = FloatRangeType::new(one, two).unwrap().display();
        assert_eq!(
            display,
            RangeDisplay::Range {
                min_value: BoundDisplay::LessThanOrEqual(one.to_string()),
                max_value: BoundDisplay::LessThanOrEqual(two.to_string()),
            },
        );

        // 1 <= .. < 2
        let display = FloatRangeType::new(one, two.next_down()).unwrap().display();
        assert_eq!(
            display,
            RangeDisplay::Range {
                min_value: BoundDisplay::LessThanOrEqual(one.to_string()),
                max_value: BoundDisplay::LessThan(two.to_string()),
            },
        );

        // 1 < .. < 2
        let display = FloatRangeType::new(one.next_up(), two.next_down())
            .unwrap()
            .display();
        assert_eq!(
            display,
            RangeDisplay::Range {
                min_value: BoundDisplay::LessThan(one.to_string()),
                max_value: BoundDisplay::LessThan(two.to_string()),
            },
        );
    }

    #[derive(Clone, Copy, Debug, Enum)]
    enum RangeTestCase {
        Same,
        EnclosedTouchingLeft,
        EnclosedTouchingRight,
        Enclosed,
        Gap,
        Touching,
        Overlap,
    }

    struct RangeTest<F: Float> {
        test_cases: EnumMap<RangeTestCase, [FloatRangeType<F>; 2]>,
        a: NotNan<F>,
        b: NotNan<F>,
        h: NotNan<F>,
        i: NotNan<F>,
        j: NotNan<F>,
        y: NotNan<F>,
        z: NotNan<F>,
    }

    fn float_range_test<F: Float>() -> RangeTest<F>
    where
        NotNan<F>: FloatStep,
    {
        let one = F::one();
        let a = NotNan::new(one).unwrap();
        let i = NotNan::new(one + one).unwrap();
        let z = NotNan::new(one + one + one).unwrap();

        let b = a.next_up();
        let h = i.next_down();
        let j = i.next_up();
        let y = z.next_down();

        RangeTest {
            test_cases: enum_map! {
                RangeTestCase::Same => [
                    FloatRangeType::new(a, z).unwrap(),
                    FloatRangeType::new(a, z).unwrap(),
                ],
                RangeTestCase::EnclosedTouchingLeft => [
                    FloatRangeType::new(a, z).unwrap(),
                    FloatRangeType::new(a, y).unwrap(),
                ],
                RangeTestCase::EnclosedTouchingRight => [
                    FloatRangeType::new(a, z).unwrap(),
                    FloatRangeType::new(b, z).unwrap(),
                ],
                RangeTestCase::Enclosed => [
                    FloatRangeType::new(a, z).unwrap(),
                    FloatRangeType::new(b, y).unwrap(),
                ],
                RangeTestCase::Touching => [
                    FloatRangeType::new(a, i).unwrap(),
                    FloatRangeType::new(j, z).unwrap(),
                ],
                RangeTestCase::Gap => [
                    FloatRangeType::new(a, h).unwrap(),
                    FloatRangeType::new(j, z).unwrap(),
                ],
                RangeTestCase::Overlap => [
                    FloatRangeType::new(a, i).unwrap(),
                    FloatRangeType::new(i, z).unwrap(),
                ],
            },
            a,
            b,
            h,
            i,
            j,
            y,
            z,
        }
    }

    #[test]
    fn test_bitor_float_range_type() {
        fn run_with<F: Float + Debug>()
        where
            NotNan<F>: FloatStep,
        {
            let RangeTest {
                test_cases,
                a,
                b: _,
                h,
                i: _,
                j,
                y: _,
                z,
            } = float_range_test::<F>();

            let expected = enum_map! {
                RangeTestCase::Same => FloatRangeBitOr::One(FloatRangeType::new(a, z).unwrap()),
                RangeTestCase::EnclosedTouchingLeft => {
                    FloatRangeBitOr::One(FloatRangeType::new(a, z).unwrap())
                }
                RangeTestCase::EnclosedTouchingRight => {
                    FloatRangeBitOr::One(FloatRangeType::new(a, z).unwrap())
                }
                RangeTestCase::Enclosed => FloatRangeBitOr::One(FloatRangeType::new(a, z).unwrap()),
                RangeTestCase::Gap => FloatRangeBitOr::Two {
                    lower: FloatRangeType::new(a, h).unwrap(),
                    higher: FloatRangeType::new(j, z).unwrap(),
                },
                RangeTestCase::Touching => FloatRangeBitOr::One(FloatRangeType::new(a, z).unwrap()),
                RangeTestCase::Overlap => FloatRangeBitOr::One(FloatRangeType::new(a, z).unwrap()),
            };

            for (test_case, [left, right]) in test_cases {
                assert_eq!(left | right, expected[test_case], "{:?}", test_case);
                assert_eq!(right | left, expected[test_case], "{:?}", test_case);
            }
        }

        run_with::<f32>();
        run_with::<f64>();
    }

    #[test]
    fn test_bitand_float_range_type() {
        fn run_with<F: Float + Debug>()
        where
            NotNan<F>: FloatStep,
        {
            let RangeTest {
                test_cases,
                a,
                b,
                h: _,
                i,
                j: _,
                y,
                z,
            } = float_range_test::<F>();

            let expected = enum_map! {
                RangeTestCase::Same => FloatRangeBitAnd::One(FloatRangeType::new(a, z).unwrap()),
                RangeTestCase::EnclosedTouchingLeft => {
                    FloatRangeBitAnd::One(FloatRangeType::new(a, y).unwrap())
                }
                RangeTestCase::EnclosedTouchingRight => {
                    FloatRangeBitAnd::One(FloatRangeType::new(b, z).unwrap())
                }
                RangeTestCase::Enclosed => FloatRangeBitAnd::One(FloatRangeType::new(b, y).unwrap()),
                RangeTestCase::Gap => FloatRangeBitAnd::None,
                RangeTestCase::Touching => FloatRangeBitAnd::None,
                RangeTestCase::Overlap => FloatRangeBitAnd::One(FloatRangeType::new(i, i).unwrap()),
            };

            for (test_case, [left, right]) in test_cases {
                assert_eq!(left & right, expected[test_case], "{:?}", test_case);
                assert_eq!(right & left, expected[test_case], "{:?}", test_case);
            }
        }

        run_with::<f32>();
        run_with::<f64>();
    }

    #[test]
    fn test_bitxor_float_range_type() {
        fn run_with<F: Float + Debug>()
        where
            NotNan<F>: FloatStep,
        {
            let RangeTest {
                test_cases,
                a,
                b: _,
                h,
                i: _,
                j,
                y: _,
                z,
            } = float_range_test::<F>();

            let expected = enum_map! {
                RangeTestCase::Same => FloatRangeBitXor::None,
                RangeTestCase::EnclosedTouchingLeft => {
                    FloatRangeBitXor::One(FloatRangeType::new(z, z).unwrap())
                }
                RangeTestCase::EnclosedTouchingRight => {
                    FloatRangeBitXor::One(FloatRangeType::new(a, a).unwrap())
                }
                RangeTestCase::Enclosed => FloatRangeBitXor::Two {
                    lower: FloatRangeType::new(a, a).unwrap(),
                    higher: FloatRangeType::new(z, z).unwrap(),
                },
                RangeTestCase::Gap => FloatRangeBitXor::Two {
                    lower: FloatRangeType::new(a, h).unwrap(),
                    higher: FloatRangeType::new(j, z).unwrap(),
                },
                RangeTestCase::Touching => FloatRangeBitXor::One(FloatRangeType::new(a, z).unwrap()),
                RangeTestCase::Overlap => FloatRangeBitXor::Two {
                    lower: FloatRangeType::new(a, h).unwrap(),
                    higher: FloatRangeType::new(j, z).unwrap(),
                },
            };

            for (test_case, [left, right]) in test_cases {
                assert_eq!(left ^ right, expected[test_case], "{:?}", test_case);
                assert_eq!(right ^ left, expected[test_case], "{:?}", test_case);
            }
        }

        run_with::<f32>();
        run_with::<f64>();
    }

    #[test]
    fn test_sub_float_range_type() {
        fn run_with<F: Float + Debug>()
        where
            NotNan<F>: FloatStep,
        {
            let RangeTest {
                test_cases,
                a,
                b: _,
                h,
                i,
                j,
                y: _,
                z,
            } = float_range_test::<F>();

            let expected = enum_map! {
                RangeTestCase::Same => FloatRangeSub::None,
                RangeTestCase::EnclosedTouchingLeft => {
                    FloatRangeSub::One(FloatRangeType::new(z, z).unwrap())
                }
                RangeTestCase::EnclosedTouchingRight => {
                    FloatRangeSub::One(FloatRangeType::new(a, a).unwrap())
                }
                RangeTestCase::Enclosed => FloatRangeSub::Two {
                    lower: FloatRangeType::new(a, a).unwrap(),
                    higher: FloatRangeType::new(z, z).unwrap(),
                },
                RangeTestCase::Gap => FloatRangeSub::One(FloatRangeType::new(a, h).unwrap()),
                RangeTestCase::Touching => FloatRangeSub::One(FloatRangeType::new(a, i).unwrap()),
                RangeTestCase::Overlap => FloatRangeSub::One(FloatRangeType::new(a, h).unwrap()),
            };

            let inverse = enum_map! {
                RangeTestCase::Same => FloatRangeSub::None,
                RangeTestCase::EnclosedTouchingLeft => FloatRangeSub::None,
                RangeTestCase::EnclosedTouchingRight => FloatRangeSub::None,
                RangeTestCase::Enclosed => FloatRangeSub::None,
                RangeTestCase::Gap => FloatRangeSub::One(FloatRangeType::new(j, z).unwrap()),
                RangeTestCase::Touching => FloatRangeSub::One(FloatRangeType::new(j, z).unwrap()),
                RangeTestCase::Overlap => FloatRangeSub::One(FloatRangeType::new(j, z).unwrap()),
            };

            for (test_case, [left, right]) in test_cases {
                assert_eq!(left - right, expected[test_case], "{:?}", test_case);
                assert_eq!(right - left, inverse[test_case], "{:?}", test_case);
            }
        }

        run_with::<f32>();
        run_with::<f64>();
    }

    #[test]
    fn test_has_gap() {
        fn run_with<F: Float + Debug>()
        where
            NotNan<F>: FloatStep,
        {
            let one = NotNan::<F>::one();
            assert!(!one.has_gap(one.next_up()));
            assert!(one.has_gap(one.next_up().next_up()));

            let inf = one / NotNan::<F>::zero();
            assert!(!inf.next_down().has_gap(inf));
            assert!(inf.next_down().next_down().has_gap(inf));

            let neg_inf = -inf;
            assert!(!neg_inf.has_gap(neg_inf.next_up()));
            assert!(neg_inf.has_gap(neg_inf.next_up().next_up()));
        }

        run_with::<f32>();
        run_with::<f64>();
    }
}
