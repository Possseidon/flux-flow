use std::sync::Arc;

use super::{DynFunction, FnFunction};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub(super) enum Impl {
    Fn(FnFunction),
    Dyn(DynFn),
    // TODO: compiled function type
    // Flux(...),
}

#[derive(Clone)]
pub(super) struct DynFn(pub(super) DynFunction);

impl std::fmt::Debug for DynFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.0)
    }
}

impl std::hash::Hash for DynFn {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // strip metadata to match PartialEq
        (Arc::as_ptr(&self.0) as *const ()).hash(state);
    }
}

impl PartialEq for DynFn {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for DynFn {}
