use std::collections::BTreeMap;

use crate::static_type::StaticType;

/// The concrete runtime type of a value.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum RuntimeType {
    Tuple(Vec<StaticType>),

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

    RangeFull,
    Range(StaticType),
    RangeFrom(StaticType),
    RangeTo(StaticType),
    RangeInclusive(StaticType),
    RangeToInclusive(StaticType),

    Array {
        ty: StaticType,
        size: usize,
    },
    Vec(StaticType),
    Deque(StaticType),
    Set(StaticType),
    Map {
        key: StaticType,
        value: StaticType,
    },

    Function {
        parameter_type: StaticType,
        return_type: StaticType,
    },

    Struct(Struct),
    NewType(NewType),

    StructType,
    NewTypeType,
    RuntimeType,
    StaticType,
}

/// Gives names to a tuple's values.
///
/// Field order does not matter; as long as two structs share both field types and names, they
/// are compatible.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Struct {
    fields: BTreeMap<String, StaticType>,
}

/// A new type that is only compatible with itself.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct NewType {
    name: String,
    ty: StaticType,
}

impl RuntimeType {
    pub const UNIT: Self = Self::Tuple(Vec::new());
}
