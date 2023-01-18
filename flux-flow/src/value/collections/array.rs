use super::super::Value;

use crate::static_type::StaticType;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypedArray {
    pub(crate) ty: StaticType,
    pub(crate) data: Box<[Value]>,
}

impl TypedArray {
    pub fn get_type(&self) -> &StaticType {
        &self.ty
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}
