use std::sync::Arc;

use super::{Value, ValueImpl};

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self(ValueImpl::Str(Arc::new(value)))
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self(ValueImpl::Str(Arc::new(value.into())))
    }
}

impl TryFrom<Value> for String {
    type Error = Value;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value.0 {
            ValueImpl::Str(value) => {
                // TODO: Use Arc::unwrap_or_clone once stable.
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
            ValueImpl::Str(value) => Ok(value),
            _ => Err(value),
        }
    }
}

impl<'a> TryFrom<&'a Value> for String {
    type Error = &'a Value;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match &value.0 {
            ValueImpl::Str(value) => Ok(value.as_ref().clone()),
            _ => Err(value),
        }
    }
}
