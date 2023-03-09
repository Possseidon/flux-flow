use super::super::Value;

use std::collections::BTreeMap;

use crate::static_type::StaticType;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedMap {
    pub key: StaticType,
    pub value: StaticType,
    pub data: BTreeMap<Value, Value>,
}

impl TypedMap {
    pub fn get_key_type(&self) -> &StaticType {
        &self.key
    }

    pub fn get_value_type(&self) -> &StaticType {
        &self.value
    }
}
