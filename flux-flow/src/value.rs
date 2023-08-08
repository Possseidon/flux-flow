pub mod number;

use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    mem::size_of,
    sync::Arc,
};

use num::BigInt;

use crate::{stack::Function, typing::Type};

/// Capable of holding any value on its own.
///
/// Can hold a small enough values in-place and falls back to allocation if necessary.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    /// Stores zero-sized values.
    Empty,
    /// Stores anything that fits in a [`ValueStorage`].
    Single(ValueStorage),
    /// Stores anything that requires multiple [`ValueStorage`]s.
    Multi(Arc<Box<[ValueStorage]>>),
    /// Stores anything that requires multiple [`FatValueStorage`]s.
    FatMulti(Arc<Box<[FatValueStorage]>>),
}

const _: () = assert!(size_of::<Value>() == 16);

/// Used by [`Value`] and [`FatValue`] as underlying storage.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ValueStorage {
    Bits(u64),
    BigInt(Arc<BigInt>),
    String(Arc<String>),
    Vec(Arc<Vec<Value>>),
    Deque(Arc<VecDeque<Value>>),
    Set(Arc<BTreeSet<Value>>),
    Map(Arc<BTreeMap<Value, Value>>),
    Function(Function),
}

const _: () = assert!(size_of::<ValueStorage>() == 16);

/// Capable of holding any value on its own.
///
/// Has a slightly bigger base footprint than [`Value`] but can store more values in-place.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FatValue {
    /// Stores zero-sized values.
    Empty,
    /// Stores anything that fits in a [`FatValueStorage`].
    Single(FatValueStorage),
    /// Stores anything that requires multiple [`ValueStorage`]s.
    Multi(Arc<[ValueStorage]>),
    /// Stores anything that requires multiple [`FatValueStorage`]s.
    FatMulti(Arc<[FatValueStorage]>),
}

const _: () = assert!(size_of::<FatValue>() == 24);

/// Can store a [`ValueStorage`] with some extra space (only in the form of bits) or completely
/// different variants that require more space to store at all.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FatValueStorage {
    Type(Type),
    /// A [`ValueStorage`] with an additional [`u64`] worth of extra bits.
    WithExtraBits {
        value: ValueStorage,
        extra_bits: u64,
    },
    /// A [`ValueStorage::String`] with only one indirection at the cost of a bigger base-footprint.
    CompactString(Arc<str>),
    /// A [`ValueStorage::Vec`] with only one indirection at the cost of a bigger base-footprint.
    CompactList(Arc<[Value]>),
}

const _: () = assert!(size_of::<FatValueStorage>() == 24);

/// How to read and write [`Value`]s.
trait ValueIO {}
