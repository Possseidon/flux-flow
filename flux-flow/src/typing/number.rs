use std::sync::Arc;

use num::BigInt;
use ordered_float::OrderedFloat;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NumberType {
    Real(RealType),
    Complex(Arc<ComplexType>),
    // TODO: any number can have a unit
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RealType {
    Integer(IntegerType),
    Rational(RationalType),
    Approx(OrderedFloat<f64>),
    ApproxRange(ApproxRange),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IntegerType {
    FastNatural(u64),
    FastNaturalRange(FastNaturalRange),
    FastInteger(i64),
    FastIntegerRange(FastIntegerRange),
    BigInteger(Arc<BigInt>),
    BigIntegerRange(BigIntegerRange),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RationalType {
    Rational(Rational),
    RationalRange(RationalRange),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Rational {
    numer: IntegerType,
    denom: IntegerType,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RationalRange {
    min: Rational,
    max: Rational,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FastNaturalRange {
    min: u64,
    max: u64,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FastIntegerRange {
    min: i64,
    max: i64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BigIntegerRange {
    min: Option<Arc<BigInt>>,
    max: Option<Arc<BigInt>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ApproxRange {
    min: OrderedFloat<f64>,
    max: OrderedFloat<f64>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ComplexType {
    re: RealType,
    im: RealType,
}
