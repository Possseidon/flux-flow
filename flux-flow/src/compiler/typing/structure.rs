use std::sync::Arc;

use super::Type;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructType {
    fields: Arc<[StructField]>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructField {
    name: Arc<str>,
    ty: Type,
}
