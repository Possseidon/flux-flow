pub mod collections;
pub mod function;
pub mod primitives;
pub mod range;
pub mod string;
pub mod tuple;

use std::sync::Arc;

use ordered_float::OrderedFloat;

use crate::{
    runtime_type::{NewType, RuntimeType, Struct},
    static_type::StaticType,
};

use self::{
    collections::{
        array::TypedArray, deque::TypedDeque, map::TypedMap, set::TypedSet, vec::TypedVec,
    },
    function::Function,
    primitives::NativeFunction,
    range::{End, Start, StartEnd},
};

// TODO: Use smartstring again to save on allocations

/// Stores a value of any type.
///
/// Conversion between Rust types and [`Value`] can be performed using [`From`] and [`TryFrom`].
///
/// Size is kept at 16 bytes, so moves are very cheap.
///
/// Additionally, clones are cheap as well, as large values are reference counted and will only be
/// cloned when mutability is desired.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value(ValueStorage);

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum ValueStorage {
    #[default]
    Unit,

    /// Stored in place to avoid additional heap allocation.
    Tuple1(Arc<Value>),
    /// Stored in place to avoid additional heap allocation.
    Tuple2(Arc<(Value, Value)>),
    /// Stored in place to avoid additional heap allocation.
    Tuple3(Arc<(Value, Value, Value)>),
    /// Stores an arbitrarily large number of tuple values.
    Tuple4OrMore(Arc<Box<[Value]>>),

    Boolean(bool),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(Arc<u128>),
    Usize(usize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(Arc<i128>),
    Isize(isize),

    F32(OrderedFloat<f32>),
    F64(OrderedFloat<f64>),

    Char(char),
    String(Arc<String>),

    RangeFull,
    Range(Arc<StartEnd>),
    RangeFrom(Arc<Start>),
    RangeTo(Arc<End>),
    RangeInclusive(Arc<StartEnd>),
    RangeToInclusive(Arc<End>),

    // TODO: Optimized Array Types
    Array(Arc<TypedArray>),

    // TODO: Optimized Vec Types
    Vec(Arc<TypedVec>),

    // TODO: Optimized Deque Types
    Deque(Arc<TypedDeque>),

    // TODO: Optimized Set Types
    Set(Arc<TypedSet>),

    // TODO: Optimized Map Types
    Map(Arc<TypedMap>),

    Function(Arc<Function>),
    NativeFunction(NativeFunction),

    Struct(Arc<(Struct, Value)>),
    UnitStruct(Arc<Struct>),

    NewType(Arc<(NewType, Value)>),
    UnitNewType(Arc<NewType>),

    StructType(Arc<Struct>),
    NewTypeType(Arc<NewType>),
    RuntimeType(Arc<RuntimeType>),
    StaticType(Arc<StaticType>),
    // ---
    // Generator(Arc<Generator>),

    // BigInt(Arc<BigInt>),

    // Rational32(Rational32),
    // Rational64(Rational64),
    // BigRational(Arc<BigRational>),
}

impl Value {
    pub fn get_type(&self) -> RuntimeType {
        match &self.0 {
            ValueStorage::Unit => RuntimeType::UNIT,

            ValueStorage::Tuple1(tuple) => RuntimeType::Tuple(vec![tuple.get_type().into()]),
            ValueStorage::Tuple2(tuple) => {
                RuntimeType::Tuple(vec![tuple.0.get_type().into(), tuple.1.get_type().into()])
            }
            ValueStorage::Tuple3(tuple) => RuntimeType::Tuple(vec![
                tuple.0.get_type().into(),
                tuple.1.get_type().into(),
                tuple.2.get_type().into(),
            ]),
            ValueStorage::Tuple4OrMore(tuple) => {
                RuntimeType::Tuple(tuple.iter().map(|value| value.get_type().into()).collect())
            }

            ValueStorage::Boolean(_) => RuntimeType::Boolean,

            ValueStorage::U8(_) => RuntimeType::U8,
            ValueStorage::U16(_) => RuntimeType::U16,
            ValueStorage::U32(_) => RuntimeType::U32,
            ValueStorage::U64(_) => RuntimeType::U64,
            ValueStorage::U128(_) => RuntimeType::U128,
            ValueStorage::Usize(_) => RuntimeType::Usize,
            ValueStorage::I8(_) => RuntimeType::I8,
            ValueStorage::I16(_) => RuntimeType::I16,
            ValueStorage::I32(_) => RuntimeType::I32,
            ValueStorage::I64(_) => RuntimeType::I64,
            ValueStorage::I128(_) => RuntimeType::I128,
            ValueStorage::Isize(_) => RuntimeType::Isize,

            ValueStorage::F32(_) => RuntimeType::F32,
            ValueStorage::F64(_) => RuntimeType::F64,

            ValueStorage::Char(_) => RuntimeType::Char,
            ValueStorage::String(_) => RuntimeType::String,

            ValueStorage::RangeFull => RuntimeType::RangeFull,
            ValueStorage::Range(range) => RuntimeType::Range(range.get_type().into()),
            ValueStorage::RangeFrom(range) => RuntimeType::RangeFrom(range.get_type().into()),
            ValueStorage::RangeTo(range) => RuntimeType::RangeTo(range.get_type().into()),
            ValueStorage::RangeInclusive(range) => {
                RuntimeType::RangeInclusive(range.get_type().into())
            }
            ValueStorage::RangeToInclusive(range) => {
                RuntimeType::RangeToInclusive(range.get_type().into())
            }

            ValueStorage::Array(array) => RuntimeType::Array {
                ty: array.get_type().clone(),
                size: array.len(),
            },
            ValueStorage::Vec(vec) => RuntimeType::Vec(vec.get_type().clone()),
            ValueStorage::Deque(deque) => RuntimeType::Deque(deque.get_type().clone()),
            ValueStorage::Set(set) => RuntimeType::Set(set.get_type().clone()),
            ValueStorage::Map(map) => RuntimeType::Map {
                key: map.get_key_type().clone(),
                value: map.get_value_type().clone(),
            },

            ValueStorage::Function(_) => RuntimeType::Function {
                parameter_type: StaticType::ANY,
                return_type: StaticType::ANY,
            },
            ValueStorage::NativeFunction(_) => RuntimeType::Function {
                parameter_type: StaticType::ANY,
                return_type: StaticType::ANY,
            },

            ValueStorage::Struct(value) => RuntimeType::Struct(value.0.clone()),
            ValueStorage::UnitStruct(value) => RuntimeType::Struct((**value).clone()),

            ValueStorage::NewType(value) => RuntimeType::NewType(value.0.clone()),
            ValueStorage::UnitNewType(value) => RuntimeType::NewType((**value).clone()),

            ValueStorage::StructType(_) => RuntimeType::StructType,
            ValueStorage::NewTypeType(_) => RuntimeType::NewTypeType,
            ValueStorage::RuntimeType(_) => RuntimeType::RuntimeType,
            ValueStorage::StaticType(_) => RuntimeType::StaticType,
        }
    }
}
