use std::{any::Any, sync::Arc};

use ordered_float::OrderedFloat;

use crate::{runtime_value::RuntimeValue, static_type::StaticType};

use super::OrderedRuntimeValue;

/// The amount of bits required to represent a value of this type.
///
/// This information can be used to pick an efficient storage for the stack or collections such as
/// lists and sets.
pub enum RuntimeValueStore<'a> {
    Bit(RuntimeValueStoreAs<'a, bool>),
    U8(RuntimeValueStoreAs<'a, u8>),
    U64(RuntimeValueStoreAs<'a, u64>),
    U128(RuntimeValueStoreAs<'a, u128>),
    Any(RuntimeValueStoreAs<'a, Arc<dyn Any>>),
}

pub struct RuntimeValueStoreAs<'a, T> {
    pub(crate) static_type: &'a StaticType,
    pub(crate) store: fn(RuntimeValue, &StaticType) -> Result<T, RuntimeValue>,
    pub(crate) load: fn(T, &StaticType) -> RuntimeValue,
}

/// Converts between [`RuntimeValue`] and its stored form `T`.
///
/// There has to be an exact one-to-one mapping between [`RuntimeValue`]s and `T`.
impl<T> RuntimeValueStoreAs<'_, T> {
    /// Converts a [`RuntimeValue`] into its stored form `T`.
    ///
    /// Returns [`Err`] with the original `value` if that `value` is of the wrong type.
    fn store(self, value: RuntimeValue) -> Result<T, RuntimeValue> {
        (self.store)(value, &self.static_type)
    }

    /// Converts stored value of type `T` into a [`RuntimeValue`].
    ///
    /// Unlike [`Self::store`], this is infallible.
    fn load(self, value: T) -> RuntimeValue {
        (self.load)(value, &self.static_type)
    }
}

pub enum OrderedRuntimeValueStore<'a> {
    Bit(OrderedRuntimeValueStoreAs<'a, bool>),
    U8(OrderedRuntimeValueStoreAs<'a, u8>),
    U64(OrderedRuntimeValueStoreAs<'a, u64>),
    U128(OrderedRuntimeValueStoreAs<'a, u128>),
    F32(OrderedRuntimeValueStoreAs<'a, OrderedFloat<f32>>),
    F64(OrderedRuntimeValueStoreAs<'a, OrderedFloat<f64>>),
    Any(OrderedRuntimeValueStoreAs<'a, Arc<dyn Any>>),
}

/// Converts between [`OrderedRuntimeValue`] and its stored form `T`.
///
/// When stored as `T`, multiple `T`s must still be ordered the same way. E.g. if some value `A` is
/// less than `B`, then the stored form of `A` e.g. `0` must also be less than the stored form of
/// `B` e.g. `1`.
///
/// This also means, signed integers cannot just be `as` casted and instead have to effectively have
/// their value range moved from the smallest negative number up to `0`:
///
/// ```
/// fn store(value: i8) -> u8 {
///     value.wrapping_sub(i8::MIN) as u8
/// }
///
/// assert!(store(-128) < store(127));
/// ```
pub struct OrderedRuntimeValueStoreAs<'a, T> {
    pub(crate) static_type: &'a StaticType,
    pub(crate) store: fn(OrderedRuntimeValue, &StaticType) -> Result<T, OrderedRuntimeValue>,
    pub(crate) load: fn(T, &StaticType) -> OrderedRuntimeValue,
}

impl<T> OrderedRuntimeValueStoreAs<'_, T> {
    /// Converts a [`OrderedRuntimeValue`] into its stored form `T`.
    ///
    /// Returns [`Err`] with the original `value` if that `value` is of the wrong type.
    fn store(self, value: OrderedRuntimeValue) -> Result<T, OrderedRuntimeValue> {
        (self.store)(value, &self.static_type)
    }

    /// Converts stored value of type `T` into a [`OrderedRuntimeValue`].
    ///
    /// Unlike [`Self::store`], this is infallible.
    fn load(self, value: T) -> OrderedRuntimeValue {
        (self.load)(value, &self.static_type)
    }
}
