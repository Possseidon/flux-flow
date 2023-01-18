use super::super::Value;

use std::collections::VecDeque;

use crate::static_type::StaticType;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedDeque {
    pub(crate) ty: StaticType,
    pub(crate) data: VecDeque<Value>,
}

impl TypedDeque {
    pub fn get_type(&self) -> &StaticType {
        &self.ty
    }
}
