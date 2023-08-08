use num::{BigInt, BigRational, BigUint, Rational64, ToPrimitive};

use std::cmp::Ordering;
use std::sync::Arc;

struct Complex(num::Complex<Real>);

#[derive(Clone, Debug)]
pub enum Real {
    Rational(Ratio),
    Approx(Approx),
}

impl Real {
    fn into_f64(self) -> f64 {
        match self {
            Self::Rational(value) => value.approx_f64(),
            Self::Approx(value) => value.into_f64(),
        }
    }
}

impl Default for Real {
    fn default() -> Self {
        Self::Rational(Ratio::Fast(Rational64::default()))
    }
}

impl PartialEq for Real {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Real {}

impl PartialOrd for Real {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Real {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Rational(lhs), Self::Rational(rhs)) => todo!(),
            (Self::Rational(lhs), Self::Approx(rhs)) => todo!(),
            (Self::Approx(lhs), Self::Rational(rhs)) => todo!(),
            (Self::Approx(lhs), Self::Approx(rhs)) => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Ratio {
    Fast(Rational64),
    Big(BigRational),
}

impl PartialEq for Ratio {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Fast(l0), Self::Fast(r0)) => l0 == r0,
            (Self::Big(l0), Self::Big(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Eq for Ratio {}

impl PartialOrd for Ratio {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        todo!()
    }
}

impl Ord for Ratio {
    fn cmp(&self, other: &Self) -> Ordering {
        todo!()
    }
}

impl Ratio {
    fn approx_f64(&self) -> f64 {
        match self {
            Self::Fast(value) => value.to_f64().unwrap_or(f64::NAN),
            Self::Big(value) => value.to_f64().unwrap_or(f64::NAN),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Integer {
    FastNatural(u64),
    Fast(i64),
    Big(Arc<BigInt>),
}

impl Integer {
    fn approx_f64(&self) -> f64 {
        match self {
            Self::FastNatural(value) => *value as f64,
            Self::Fast(value) => *value as f64,
            Self::Big(value) => value
                .to_f64()
                .expect("BigInt to f64 conversion should not fail"),
        }
    }
}

#[derive(Clone, Debug)]
enum Natural {
    Fast(u64),
    Big(Arc<BigUint>),
}

impl PartialEq for Natural {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Natural {}

impl PartialOrd for Natural {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Natural {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Natural::Fast(lhs), Natural::Fast(rhs)) => lhs.cmp(rhs),
            (Natural::Fast(lhs), Natural::Big(rhs)) => {
                rhs.to_u64().map_or(Ordering::Less, |rhs| lhs.cmp(&rhs))
            }
            (Natural::Big(lhs), Natural::Fast(rhs)) => {
                lhs.to_u64().map_or(Ordering::Greater, |lhs| lhs.cmp(rhs))
            }
            (Natural::Big(lhs), Natural::Big(rhs)) => lhs.cmp(rhs),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Approx {
    F32(f32),
    F64(f64),
}

impl Approx {
    fn into_f64(self) -> f64 {
        match self {
            Self::F32(value) => value as f64,
            Self::F64(value) => value,
        }
    }
}

impl PartialEq for Approx {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other) == Some(Ordering::Equal)
    }
}

impl PartialOrd for Approx {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::F32(lhs), Self::F32(rhs)) => lhs.partial_cmp(rhs),
            _ => self.into_f64().partial_cmp(&other.into_f64()),
        }
    }
}
