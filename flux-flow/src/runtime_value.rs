mod list;
mod map;
mod number;
mod set;
mod string;
mod r#struct;

use std::time::Instant;

use ordered_float::OrderedFloat;

// TODO: structs and lists should have special variants that can reference directly into the stack
// TODO: special variants for compound types that can be represented as raw bits
// TODO: special variant for sets that can be represented as bitsets
// TODO: sets with bitset storage should be able to reference into the stack as well

/// A type that can store any runtime value.
#[derive(Clone, Debug, Eq)]
pub enum RuntimeValue {
    Ordered(OrderedRuntimeValue),
    List(RuntimeList),
    Map(RuntimeMap),
    Struct(RuntimeStruct),
    Function(RuntimeFunction),
    Distinct(RuntimeDistinct),
}

impl RuntimeValue {
    pub const UNIT: Self = Self::Ordered(OrderedRuntimeValue::UNIT);
}

impl Default for RuntimeValue {
    fn default() -> Self {
        Self::UNIT
    }
}

impl std::hash::Hash for RuntimeValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            RuntimeValue::Ordered(ordered) => ordered.hash(state),
            RuntimeValue::List(list) => {
                ValueKind::List.hash(state);
                list.hash(state);
            }
            RuntimeValue::Map(map) => {
                ValueKind::Map.hash(state);
                map.hash(state);
            }
            RuntimeValue::Struct(r#struct) => {
                ValueKind::Struct.hash(state);
                r#struct.hash(state);
            }
            RuntimeValue::Function(function) => {
                ValueKind::Function.hash(state);
                function.hash(state);
            }
            RuntimeValue::Distinct(distinct) => {
                ValueKind::Distinct.hash(state);
                distinct.hash(state);
            }
        }
    }
}

impl PartialEq for RuntimeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RuntimeValue::Ordered(lhs), RuntimeValue::Ordered(rhs)) => lhs == rhs,
            (RuntimeValue::List(lhs), RuntimeValue::List(rhs)) => lhs == rhs,
            (RuntimeValue::Map(lhs), RuntimeValue::Map(rhs)) => lhs == rhs,
            (RuntimeValue::Struct(lhs), RuntimeValue::Struct(rhs)) => lhs == rhs,
            (RuntimeValue::Function(lhs), RuntimeValue::Function(rhs)) => lhs == rhs,
            (RuntimeValue::Distinct(lhs), RuntimeValue::Distinct(rhs)) => lhs == rhs,

            (RuntimeValue::Ordered(lhs), rhs) => lhs == rhs,
            (lhs, RuntimeValue::Ordered(rhs)) => lhs == rhs,

            _ => false,
        }
    }
}

impl PartialEq<OrderedRuntimeValue> for RuntimeValue {
    fn eq(&self, other: &OrderedRuntimeValue) -> bool {
        match (self, other) {
            (Self::Ordered(lhs), rhs) => lhs == rhs,
            (Self::List(lhs), OrderedRuntimeValue::List(rhs)) => lhs == rhs,
            (Self::Map(lhs), OrderedRuntimeValue::Map(rhs)) => lhs == rhs,
            (Self::Struct(lhs), OrderedRuntimeValue::Struct(rhs)) => lhs == rhs,
            (Self::Distinct(lhs), OrderedRuntimeValue::Distinct(rhs)) => lhs == rhs,

            _ => false,
        }
    }
}

impl From<OrderedRuntimeValue> for RuntimeValue {
    fn from(value: OrderedRuntimeValue) -> Self {
        Self::Ordered(value)
    }
}

/// A type that can store any ordered runtime value.
///
/// This excludes functions as well as compound types if they contain an unordered type.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum OrderedRuntimeValue {
    Boolean(RuntimeBoolean),
    Number(RuntimeNumber),
    F32(OrderedFloat<f32>),
    F64(OrderedFloat<f64>),
    Instant(Instant),
    Uuid(u128), // TODO: use uuid crate
    Char(char),
    String(RuntimeString),
    List(OrderedRuntimeList),
    Set(RuntimeSet),
    Map(OrderedRuntimeMap),
    Struct(OrderedRuntimeStruct),
    Meta(RuntimeMeta),
    Distinct(OrderedRuntimeDistinct),
}

impl OrderedRuntimeValue {
    pub const UNIT: Self = Self::Struct(OrderedRuntimeStruct::UNIT);
}

impl Default for OrderedRuntimeValue {
    fn default() -> Self {
        Self::UNIT
    }
}

impl std::hash::Hash for OrderedRuntimeValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            OrderedRuntimeValue::Boolean(value) => {
                ValueKind::Boolean.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::Number(value) => {
                ValueKind::Number.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::F32(value) => {
                ValueKind::F32.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::F64(value) => {
                ValueKind::F64.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::Instant(value) => {
                ValueKind::Instant.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::Uuid(value) => {
                ValueKind::Uuid.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::Char(value) => {
                ValueKind::Char.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::String(value) => {
                ValueKind::String.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::List(value) => {
                ValueKind::List.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::Set(value) => {
                ValueKind::Set.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::Map(value) => {
                ValueKind::Map.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::Struct(value) => {
                ValueKind::Struct.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::Meta(value) => {
                ValueKind::Meta.hash(state);
                value.hash(state);
            }
            OrderedRuntimeValue::Distinct(value) => {
                ValueKind::Distinct.hash(state);
                value.hash(state);
            }
        }
    }
}

impl PartialEq<RuntimeValue> for OrderedRuntimeValue {
    fn eq(&self, other: &RuntimeValue) -> bool {
        other == self
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ValueKind {
    Boolean,
    Number,
    F32,
    F64,
    Instant,
    Uuid,
    Char,
    String,
    List,
    Set,
    Map,
    Struct,
    Function,
    Meta,
    Distinct,
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum RuntimeBoolean {
    #[default]
    False,
    Maybe,
    True,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct RuntimeNumber {
    value: number::NumberImpl,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct RuntimeString {
    string: string::StringImpl,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct RuntimeList {
    list: list::Impl,
}

impl PartialEq<OrderedRuntimeList> for RuntimeList {
    fn eq(&self, other: &OrderedRuntimeList) -> bool {
        self.list == other.list
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OrderedRuntimeList {
    list: list::OrderedImpl,
}

impl PartialEq<RuntimeList> for OrderedRuntimeList {
    fn eq(&self, other: &RuntimeList) -> bool {
        other == self
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct RuntimeSet {
    set: set::Impl,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct RuntimeMap {
    map: map::Impl,
}

impl PartialEq<OrderedRuntimeMap> for RuntimeMap {
    fn eq(&self, other: &OrderedRuntimeMap) -> bool {
        self.map == other.map
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OrderedRuntimeMap {
    map: map::OrderedImpl,
}

impl PartialEq<RuntimeMap> for OrderedRuntimeMap {
    fn eq(&self, other: &RuntimeMap) -> bool {
        other == self
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct RuntimeStruct {
    r#struct: r#struct::Impl,
}

impl RuntimeStruct {
    pub const UNIT: Self = Self {
        r#struct: r#struct::Impl::Empty,
    };
}

impl PartialEq<OrderedRuntimeStruct> for RuntimeStruct {
    fn eq(&self, other: &OrderedRuntimeStruct) -> bool {
        self.r#struct == other.r#struct
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OrderedRuntimeStruct {
    r#struct: r#struct::OrderedImpl,
}

impl OrderedRuntimeStruct {
    pub const UNIT: Self = Self {
        r#struct: r#struct::OrderedImpl::Empty,
    };
}

impl PartialEq<RuntimeStruct> for OrderedRuntimeStruct {
    fn eq(&self, other: &RuntimeStruct) -> bool {
        other == self
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RuntimeFunction {}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct RuntimeMeta {}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RuntimeDistinct {}

impl PartialEq<OrderedRuntimeDistinct> for RuntimeDistinct {
    fn eq(&self, other: &OrderedRuntimeDistinct) -> bool {
        todo!()
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct OrderedRuntimeDistinct {}

impl PartialEq<RuntimeDistinct> for OrderedRuntimeDistinct {
    fn eq(&self, other: &RuntimeDistinct) -> bool {
        other == self
    }
}
