pub mod function;

use std::{mem::size_of, sync::Arc};

use bitvec::vec::BitVec;
use ordered_float::OrderedFloat;

// TODO: Use smartstring again to save on allocations

/// Stores a value of any type.
///
/// Size is kept at 16 bytes, so moves are very cheap.
///
/// Additionally, clones are cheap as well, as large values are reference counted and will only be
/// cloned when mutability is desired.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Value {
    Bool(bool),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(Arc<u128>),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(Arc<i128>),
    F32(OrderedFloat<f32>),
    F64(OrderedFloat<f64>),
    Char(char),
    // Str(Arc<String>),
    // StrSlice(Arc<StrSlice>),
    // Box(Arc<Vec<Value>>),
    // Vec(Arc<Vec<Value>>),
    // Deque(Arc<VecDeque<Value>>),
    // Set(Arc<BTreeSet<Value>>),
    // Map(Arc<BTreeMap<Value, Value>>),
    // Function(Arc<Function>),
    // NativeFunction(NativeFunction),
    // Generator(Arc<Generator>),
    // RawType(Arc<MonoType>),
    // RuntimeType(Arc<RuntimeType>),
    // StaticType(Arc<StaticType>),
}

const _: () = assert!(size_of::<Value>() <= 16, "Value should at most 16 bytes");

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ValueKind {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    Char,
    // Str,
    // StrSlice,
    // Box,
    // Vec,
    // Deque,
    // Set,
    // Map,
    // Function,
    // NativeFunction,
    // Generator,
    // RawType,
    // RuntimeType,
    // StaticType,
}

/// A stack that efficiently packs primitive values, by utilizing separate [`Vec`]s.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ValueStack {
    /// Holds primitive values that can be represented by one or more bits ([bool]).
    bits: BitVec,
    /// Holds primitive values that have an alignment of 1 ([u8] and [i8]).
    u8s: Vec<u8>,
    /// Holds primitive values that have an alignment of 2 ([u16] and [i16]).
    u16s: Vec<u16>,
    /// Holds primitive values that have an alignment of 4 ([u32], [i32], [f32] and [char]).
    u32s: Vec<u32>,
    /// Holds primitive values that have an alignment of 8 ([u64], [i64], [f64], [u128] and [i128]).
    u64s: Vec<u64>,
    /// Used for anything else or if the concrete type of a primitive must be known at runtime.
    values: Vec<Value>,
}

// impl Stack {
//     fn push(&mut self, value: Value) {
//         match value {
//             Value::Bool(value) => self.push_bool(value),
//             Value::U8(value) => self.push_u8(value),
//             Value::U16(value) => self.push_u16(value),
//             Value::U32(value) => self.push_u32(value),
//             Value::U64(value) => self.push_u64(value),
//             Value::U128(value) => self.push_u128(*value),
//             Value::I8(value) => self.push_i8(value),
//             Value::I16(value) => self.push_i16(value),
//             Value::I32(value) => self.push_i32(value),
//             Value::I64(value) => self.push_i64(value),
//             Value::I128(value) => self.push_i128(*value),
//             Value::F32(value) => self.push_f32(value.0),
//             Value::F64(value) => self.push_f64(value.0),
//             Value::Char(value) => self.u32s.push(value as u32),
//             value => self.values.push(value),
//         }
//     }

//     fn push_bool(&mut self, value: bool) {
//         self.bits.push(value);
//     }

//     fn push_u8(&mut self, value: u8) {
//         self.u8s.push(value);
//     }

//     fn push_u16(&mut self, value: u16) {
//         self.u16s.push(value);
//     }

//     fn push_u32(&mut self, value: u32) {
//         self.u32s.push(value);
//     }

//     fn push_u64(&mut self, value: u64) {
//         self.u64s.push(value);
//     }

//     fn push_u128(&mut self, value: u128) {
//         self.u128s.push(value);
//     }

//     fn push_i8(&mut self, value: i8) {
//         self.u8s.push(value as u8);
//     }

//     fn push_i16(&mut self, value: i16) {
//         self.u16s.push(value as u16);
//     }

//     fn push_i32(&mut self, value: i32) {
//         self.u32s.push(value as u32);
//     }

//     fn push_i64(&mut self, value: i64) {
//         self.u64s.push(value as u64);
//     }

//     fn push_i128(&mut self, value: i128) {
//         self.u128s.push(value as u128);
//     }

//     fn push_f32(&mut self, value: f32) {
//         self.u32s.push(value.to_bits());
//     }

//     fn push_f64(&mut self, value: f64) {
//         self.u64s.push(value.to_bits());
//     }

//     fn push_value(&mut self, value: Value) {
//         self.values.push(value);
//     }

//     fn pop(&mut self, kind: ValueKind) -> Value {
//         match kind {
//             ValueKind::Bool => Value::Bool(self.pop_bool()),
//             ValueKind::U8 => Value::U8(self.pop_u8()),
//             ValueKind::U16 => Value::U16(self.pop_u16()),
//             ValueKind::U32 => Value::U32(self.pop_u32()),
//             ValueKind::U64 => Value::U64(self.pop_u64()),
//             ValueKind::U128 => Value::U128(Arc::new(self.pop_u128())),
//             ValueKind::I8 => Value::I8(self.pop_i8()),
//             ValueKind::I16 => Value::I16(self.pop_i16()),
//             ValueKind::I32 => Value::I32(self.pop_i32()),
//             ValueKind::I64 => Value::I64(self.pop_i64()),
//             ValueKind::I128 => Value::I128(Arc::new(self.pop_i128())),
//             ValueKind::F32 => Value::F32(OrderedFloat(self.pop_f32())),
//             ValueKind::F64 => Value::F64(OrderedFloat(self.pop_f64())),
//             ValueKind::Char => Value::Char(self.pop_char()),
//         }
//     }

//     fn pop_bool(&mut self) -> bool {
//         self.bits.pop().expect("bits should not be empty")
//     }

//     fn pop_u8(&mut self) -> u8 {
//         self.u8s.pop().expect("u8s should not be empty")
//     }

//     fn pop_u16(&mut self) -> u16 {
//         self.u16s.pop().expect("u16s should not be empty")
//     }

//     fn pop_u32(&mut self) -> u32 {
//         self.u32s.pop().expect("u32s should not be empty")
//     }

//     fn pop_u64(&mut self) -> u64 {
//         self.u64s.pop().expect("u64s should not be empty")
//     }

//     fn pop_u128(&mut self) -> u128 {
//         self.u128s.pop().expect("u128s should not be empty")
//     }

//     fn pop_i8(&mut self) -> i8 {
//         self.u8s.pop().expect("u8s should not be empty") as i8
//     }

//     fn pop_i16(&mut self) -> i16 {
//         self.u16s.pop().expect("u16s should not be empty") as i16
//     }

//     fn pop_i32(&mut self) -> i32 {
//         self.u32s.pop().expect("u32s should not be empty") as i32
//     }

//     fn pop_i64(&mut self) -> i64 {
//         self.u64s.pop().expect("u64s should not be empty") as i64
//     }

//     fn pop_i128(&mut self) -> i128 {
//         self.u128s.pop().expect("u128s should not be empty") as i128
//     }

//     fn pop_f32(&mut self) -> f32 {
//         f32::from_bits(self.u32s.pop().expect("u32s should not be empty"))
//     }

//     fn pop_f64(&mut self) -> f64 {
//         f64::from_bits(self.u64s.pop().expect("u64s should not be empty"))
//     }

//     fn pop_char(&mut self) -> char {
//         char::from_u32(self.u32s.pop().expect("chars should not be empty"))
//             .expect("char should be valid")
//     }

//     fn pop_value(&mut self) -> Value {
//         self.values.pop().expect("values should not be empty")
//     }
// }
