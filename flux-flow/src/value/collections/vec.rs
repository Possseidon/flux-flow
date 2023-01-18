use super::super::Value;

use crate::static_type::StaticType;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedVec {
    pub(crate) ty: StaticType,
    pub(crate) data: Vec<Value>,
}

impl TypedVec {
    pub fn get_type(&self) -> &StaticType {
        &self.ty
    }
}
