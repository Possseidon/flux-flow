use std::sync::Arc;

use super::Type;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StringType {
    Values(Option<Arc<[Type]>>),
    Sized(SizedStringType),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SizedStringType {
    ty: Type,
    min_size: usize,
    max_size: usize,
}
