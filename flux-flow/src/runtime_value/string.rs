use std::sync::Arc;

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum StringImpl {
    #[default]
    Empty,
    NonEmpty(Arc<String>),
}
