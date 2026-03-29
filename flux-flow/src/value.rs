pub mod collections;
pub mod function;
pub mod primitives;
pub mod range;
pub mod string;
pub mod tuple;
pub mod typed_function;

use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    sync::Arc,
};

use ordered_float::OrderedFloat;

use crate::{
    runtime_type::{NamedStruct, RuntimeType, TypedStruct},
    static_type::StaticType,
};

use self::{
    function::Function,
    primitives::NativeFunction,
    range::{End, Start, StartEnd},
};

// TODO: Use smartstring again to save on allocations

// TODO: Deeply nested Value can cause a stack overflow on drop.

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
    Tuple1(Arc<[Value; 1]>),
    /// Stored in place to avoid additional heap allocation.
    Tuple2(Arc<[Value; 2]>),
    /// Stored in place to avoid additional heap allocation.
    Tuple3(Arc<[Value; 3]>),
    /// Stores an arbitrarily large number of tuple values.
    Tuple4OrMore(Arc<Box<[Value]>>),

    TypedTuple1(Arc<([StaticType; 1], [Value; 1])>),
    TypedTuple2(Arc<([StaticType; 2], [Value; 2])>),
    TypedTuple3(Arc<([StaticType; 3], [Value; 3])>),
    TypedTuple4OrMore(Arc<(Box<[StaticType; 2]>, Box<[Value; 2]>)>),

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
    StringSlice(Arc<(Arc<String>, (Option<usize>, Option<usize>))>),

    RangeFull,
    Range(Arc<StartEnd>),
    RangeFrom(Arc<Start>),
    RangeTo(Arc<End>),
    RangeInclusive(Arc<StartEnd>),
    RangeToInclusive(Arc<End>),

    TypedRange(Arc<(StaticType, StartEnd)>),
    TypedRangeFrom(Arc<(StaticType, Start)>),
    TypedRangeTo(Arc<(StaticType, End)>),
    TypedRangeInclusive(Arc<(StaticType, StartEnd)>),
    TypedRangeToInclusive(Arc<(StaticType, End)>),

    UnitStruct,
    Struct1(Arc<Value>),
    Struct2(Arc<[Value; 2]>),
    Struct3(Arc<[Value; 3]>),
    Struct4OrMore(Arc<Box<[Value]>>),

    NamedStruct1(Arc<(NamedStruct, Value)>),
    NamedStruct2(Arc<(NamedStruct, [Value; 2])>),
    NamedStruct3(Arc<(NamedStruct, [Value; 3])>),
    NamedStruct4OrMore(Arc<(NamedStruct, Box<[Value]>)>),

    TypedStruct1(Arc<(TypedStruct, Value)>),
    TypedStruct2(Arc<(TypedStruct, [Value; 2])>),
    TypedStruct3(Arc<(TypedStruct, [Value; 3])>),
    TypedStruct4OrMore(Arc<(TypedStruct, Box<[Value]>)>),

    // TODO: Optimized Array Types
    EmptyArray,
    Array(Arc<Box<[Value]>>),
    TypedArray(Arc<(StaticType, Box<[Value]>)>),

    // TODO: Optimized Vec Types
    Vec(Arc<Vec<Value>>),
    TypedVec(Arc<(StaticType, Vec<Value>)>),

    // TODO: Optimized Deque Types
    Deque(Arc<VecDeque<Value>>),
    TypedDeque(Arc<(StaticType, VecDeque<Value>)>),

    // TODO: Optimized Set Types
    Set(Arc<BTreeSet<Value>>),
    TypedSet(Arc<(StaticType, BTreeSet<Value>)>),

    // TODO: Optimized Map Types
    Map(Arc<BTreeMap<Value, Value>>),
    TypedMap(Arc<(StaticType, StaticType, BTreeMap<Value, Value>)>),

    Function(Arc<Function>),
    // TypedFunction(Arc<TypedFunction>),
    NativeFunction(NativeFunction),
    // TypedNativeFunction(Arc<TypedNativeFunction>),

    // TODO:
    // UnitNewType(Arc<NewTypeId>),
    // UnitStructNewType(Arc<NewTypeId>),
    // NewType(Arc<(NewType, Value)>),

    // StructLayout(Arc<NamedStruct>),
    // NewTypeType(Arc<NewType>),
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
            ValueStorage::Unit => RuntimeType::Unit,

            ValueStorage::Tuple1(tuple) => RuntimeType::Tuple(Box::new([tuple[0].get_type()])),
            ValueStorage::Tuple2(tuple) => {
                RuntimeType::Tuple(tuple.iter().map(Self::get_type).collect())
            }
            ValueStorage::Tuple3(tuple) => {
                RuntimeType::Tuple(tuple.iter().map(Self::get_type).collect())
            }
            ValueStorage::Tuple4OrMore(tuple) => {
                RuntimeType::Tuple(tuple.iter().map(Self::get_type).collect())
            }

            ValueStorage::TypedTuple1(tuple) => RuntimeType::TypedTuple(Box::new(tuple.0.clone())),
            ValueStorage::TypedTuple2(tuple) => RuntimeType::TypedTuple(Box::new(tuple.0.clone())),
            ValueStorage::TypedTuple3(tuple) => RuntimeType::TypedTuple(Box::new(tuple.0.clone())),
            ValueStorage::TypedTuple4OrMore(tuple) => RuntimeType::TypedTuple(tuple.0.clone()),

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
            ValueStorage::StringSlice(_) => RuntimeType::StringSlice,

            ValueStorage::RangeFull => RuntimeType::RangeFull,
            ValueStorage::Range(range) => RuntimeType::Range(Box::new(range.get_type())),
            ValueStorage::RangeFrom(range) => RuntimeType::RangeFrom(Box::new(range.get_type())),
            ValueStorage::RangeTo(range) => RuntimeType::RangeTo(Box::new(range.get_type())),
            ValueStorage::RangeInclusive(range) => {
                RuntimeType::RangeInclusive(Box::new(range.get_type()))
            }
            ValueStorage::RangeToInclusive(range) => {
                RuntimeType::RangeToInclusive(Box::new(range.get_type()))
            }

            ValueStorage::TypedRange(range) => RuntimeType::TypedRange(range.0.clone()),
            ValueStorage::TypedRangeFrom(range) => RuntimeType::TypedRange(range.0.clone()),
            ValueStorage::TypedRangeTo(range) => RuntimeType::TypedRange(range.0.clone()),
            ValueStorage::TypedRangeInclusive(range) => RuntimeType::TypedRange(range.0.clone()),
            ValueStorage::TypedRangeToInclusive(range) => RuntimeType::TypedRange(range.0.clone()),

            ValueStorage::UnitStruct => RuntimeType::UnitStruct,

            ValueStorage::Struct1(_) => todo!(),
            ValueStorage::Struct2(_) => todo!(),
            ValueStorage::Struct3(_) => todo!(),
            ValueStorage::Struct4OrMore(_) => todo!(),

            ValueStorage::NamedStruct1(_) => todo!(),
            ValueStorage::NamedStruct2(_) => todo!(),
            ValueStorage::NamedStruct3(_) => todo!(),
            ValueStorage::NamedStruct4OrMore(_) => todo!(),

            ValueStorage::TypedStruct1(_) => todo!(),
            ValueStorage::TypedStruct2(_) => todo!(),
            ValueStorage::TypedStruct3(_) => todo!(),
            ValueStorage::TypedStruct4OrMore(_) => todo!(),

            ValueStorage::EmptyArray => todo!(),
            ValueStorage::Array(_) => todo!(),
            ValueStorage::TypedArray(_) => todo!(),

            ValueStorage::Vec(_) => todo!(),
            ValueStorage::TypedVec(_) => todo!(),

            ValueStorage::Deque(_) => todo!(),
            ValueStorage::TypedDeque(_) => todo!(),

            ValueStorage::Set(_) => todo!(),
            ValueStorage::TypedSet(_) => todo!(),

            ValueStorage::Map(_) => todo!(),
            ValueStorage::TypedMap(_) => todo!(),

            ValueStorage::Function(_) => todo!(),
            ValueStorage::NativeFunction(_) => todo!(),

            ValueStorage::RuntimeType(_) => todo!(),
            ValueStorage::StaticType(_) => todo!(),
        }
    }
}
