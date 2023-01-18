use std::{
    ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
    sync::Arc,
};

use crate::runtime_type::RuntimeType;

use super::{Value, ValueStorage};

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct StartEnd {
    start: Value,
    end: Value,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Start {
    start: Value,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct End {
    end: Value,
}

impl StartEnd {
    pub(crate) fn get_type(&self) -> RuntimeType {
        self.start.get_type()
    }
}

impl Start {
    pub(crate) fn get_type(&self) -> RuntimeType {
        self.start.get_type()
    }
}

impl End {
    pub(crate) fn get_type(&self) -> RuntimeType {
        self.end.get_type()
    }
}

impl From<RangeFull> for Value {
    fn from(_: RangeFull) -> Self {
        Self(ValueStorage::RangeFull)
    }
}

impl<T: Into<Value>> From<Range<T>> for Value {
    fn from(range: Range<T>) -> Self {
        Self(ValueStorage::Range(Arc::new(StartEnd {
            start: range.start.into(),
            end: range.end.into(),
        })))
    }
}

impl<T: Into<Value>> From<RangeFrom<T>> for Value {
    fn from(range: RangeFrom<T>) -> Self {
        Self(ValueStorage::RangeFrom(Arc::new(Start {
            start: range.start.into(),
        })))
    }
}

impl<T: Into<Value>> From<RangeTo<T>> for Value {
    fn from(range: RangeTo<T>) -> Self {
        Self(ValueStorage::RangeTo(Arc::new(End {
            end: range.end.into(),
        })))
    }
}

impl<T: Into<Value>> From<RangeInclusive<T>> for Value {
    fn from(range: RangeInclusive<T>) -> Self {
        let (start, end) = range.into_inner();
        Self(ValueStorage::RangeInclusive(Arc::new(StartEnd {
            start: start.into(),
            end: end.into(),
        })))
    }
}

impl<T: Into<Value>> From<RangeToInclusive<T>> for Value {
    fn from(range: RangeToInclusive<T>) -> Self {
        Self(ValueStorage::RangeToInclusive(Arc::new(End {
            end: range.end.into(),
        })))
    }
}
