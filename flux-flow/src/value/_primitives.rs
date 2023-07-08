use ordered_float::OrderedFloat;

use crate::static_type::StaticType;

use super::{Value, ValueImpl};

macro_rules! impl_from_for {
    { $( $Variant:ident($T:ty), )* } => {
        $(
            impl From<$T> for Value {
                fn from(value: $T) -> Self {
                    Value(ValueStorage::$Variant(value))
                }
            }

            impl TryFrom<Value> for $T {
                type Error = Value;

                fn try_from(value: Value) -> Result<Self, Self::Error> {
                    match value.0 {
                        ValueStorage::$Variant(value) => Ok(value),
                        _ => Err(value),
                    }
                }
            }

            impl<'a> TryFrom<&'a Value> for &'a $T {
                type Error = &'a Value;

                fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
                    match &value.0 {
                        ValueStorage::$Variant(value) => Ok(value),
                        _ => Err(value),
                    }
                }
            }

            impl<'a> TryFrom<&'a Value> for $T {
                type Error = &'a Value;

                fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
                    if let Value(ValueStorage::$Variant(value)) = value {
                        Ok(*value)
                    } else {
                        Err(value)
                    }
                }
            }
        )*
    };
}

pub type NativeFunction = fn(Value) -> Value;

pub struct TypedNativeFunction {
    argument_type: StaticType,
    result_type: StaticType,
    function: NativeFunction,
}

impl_from_for! {
    Boolean(bool),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    // U128(Arc<u128>),
    Usize(usize),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    // I128(Arc<i128>),
    Isize(isize),

    F32(OrderedFloat<f32>),
    F64(OrderedFloat<f64>),

    Char(char),

    NativeFunction(NativeFunction),
}
