use super::Type;

use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructType {
    fields: Option<Arc<[StructField]>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    name: Arc<str>,
    ty: Type,
}
