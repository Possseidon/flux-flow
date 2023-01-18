use super::super::Value;

use std::collections::BTreeSet;

use crate::static_type::StaticType;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedSet {
    pub(crate) ty: StaticType,
    pub(crate) data: BTreeSet<Value>,
}

impl TypedSet {
    pub fn get_type(&self) -> &StaticType {
        &self.ty
    }
}
