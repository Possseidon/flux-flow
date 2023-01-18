use std::collections::BTreeSet;

use crate::{runtime_type::RuntimeType, value::Value};

// TODO: Use infset
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum InfBTreeSet<T> {
    Union(BTreeSet<T>),
    Complement(BTreeSet<T>),
}

impl<T: Default> Default for InfBTreeSet<T> {
    fn default() -> Self {
        Self::Union(BTreeSet::new())
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StaticType(InfBTreeSet<RuntimeType>);

impl StaticType {
    pub const ANY: Self = Self(InfBTreeSet::Complement(BTreeSet::new()));
    pub const NEVER: Self = Self(InfBTreeSet::Union(BTreeSet::new()));

    pub fn unit() -> Self {
        Self(InfBTreeSet::Union(BTreeSet::from([RuntimeType::UNIT])))
    }

    pub fn check_value(&self, value: &Value) -> bool {
        todo!()
    }
}

impl From<RuntimeType> for StaticType {
    fn from(value: RuntimeType) -> Self {
        Self(InfBTreeSet::Union([value].into()))
    }
}
