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

/// The static type of a variable during compilation.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StaticType(InfBTreeSet<RuntimeType>);

impl StaticType {
    /// Allows a variable to have any concrete type.
    pub const ANY: Self = Self(InfBTreeSet::Complement(BTreeSet::new()));
    /// A variable of this type can never be constructed.
    pub const NEVER: Self = Self(InfBTreeSet::Union(BTreeSet::new()));

    pub fn unit() -> Self {
        Self(InfBTreeSet::Union(BTreeSet::from([RuntimeType::Unit])))
    }

    pub fn check_value(&self, _value: &Value) -> bool {
        // self.0.contains(value.get_type())
        todo!()
    }
}

impl From<RuntimeType> for StaticType {
    fn from(value: RuntimeType) -> Self {
        Self(InfBTreeSet::Union([value].into()))
    }
}
