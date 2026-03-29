use std::collections::{BTreeMap, BTreeSet};

use crate::static_type::StaticType;

/// The concrete runtime type of a value.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum RuntimeType {
    #[default]
    Unit,

    Tuple(Box<[RuntimeType]>),
    TypedTuple(Box<[StaticType]>),

    Boolean,

    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,

    F32,
    F64,

    Char,
    String,
    StringSlice,

    RangeFull,
    Range(Box<RuntimeType>),
    RangeFrom(Box<RuntimeType>),
    RangeTo(Box<RuntimeType>),
    RangeInclusive(Box<RuntimeType>),
    RangeToInclusive(Box<RuntimeType>),

    TypedRange(StaticType),
    TypedRangeFrom(StaticType),
    TypedRangeTo(StaticType),
    TypedRangeInclusive(StaticType),
    TypedRangeToInclusive(StaticType),

    UnitStruct,
    Struct(Box<[RuntimeType]>),
    NamedStruct(NamedStruct),
    TypedStruct(TypedStruct),

    EmptyArray,
    Array {
        ty: Box<RuntimeType>,
        size: usize,
    },
    TypedArray {
        ty: StaticType,
        size: usize,
    },

    EmptyVec,
    Vec(Box<RuntimeType>),
    TypedVec(StaticType),

    EmptyDeque,
    Deque(Box<RuntimeType>),
    TypedDeque(StaticType),

    EmptySet,
    Set(Box<RuntimeType>),
    TypedSet(StaticType),

    EmptyMap,
    Map {
        key: Box<RuntimeType>,
        value: Box<RuntimeType>,
    },
    TypedMap {
        key: StaticType,
        value: StaticType,
    },

    Function,
    TypedFunction {
        argument_type: StaticType,
        result_type: StaticType,
    },

    // TODO:
    // UnitNewType(NewType),
    // UnitStructNewType(NewType),
    // NewType(NewType),

    // StructLayout(NamedStruct),
    // TypedStructLayout(TypedStruct),
    // NewTypeType,
    RuntimeType,
    StaticType,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NamedStruct {
    order: BTreeSet<String>,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedStruct {
    fields: BTreeMap<String, StaticType>,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NewTypeId;

/// A new type that is only compatible with itself.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NewType {
    /// An id that is unique to this type.
    id: NewTypeId,
    ty: StaticType,
}
