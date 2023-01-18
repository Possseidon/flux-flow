use std::sync::Arc;

use super::{Value, ValueStorage};

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self(ValueStorage::String(Arc::new(value)))
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self(ValueStorage::String(Arc::new(value.into())))
    }
}

impl TryFrom<Value> for String {
    type Error = Value;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value.0 {
            ValueStorage::String(value) => {
                Ok(Arc::try_unwrap(value).unwrap_or_else(|arc| (*arc).clone()))
            }
            _ => Err(value),
        }
    }
}

impl<'a> TryFrom<&'a Value> for &'a str {
    type Error = &'a Value;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match &value.0 {
            ValueStorage::String(value) => Ok(value),
            _ => Err(value),
        }
    }
}

impl<'a> TryFrom<&'a Value> for String {
    type Error = &'a Value;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match &value.0 {
            ValueStorage::String(value) => Ok(value.as_ref().clone()),
            _ => Err(value),
        }
    }
}
