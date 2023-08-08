use super::Type;

use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ListType {
    Values(Option<Arc<[Type]>>),
    Sized(SizedListType),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SizedListType {
    ty: Type,
    min_size: usize,
    max_size: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SetType {
    ty: Type,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MapType {
    key: Type,
    val: Type,
}
