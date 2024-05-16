use std::{cmp::Ordering, sync::Arc};

#[derive(Clone, Debug, Default, Eq)]
pub(super) enum StringImpl {
    #[default]
    Empty,
    NonEmpty(Arc<String>),
}

impl StringImpl {
    pub fn as_str(&self) -> &str {
        match self {
            StringImpl::Empty => "",
            StringImpl::NonEmpty(string) => string,
        }
    }
}

impl std::hash::Hash for StringImpl {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl PartialEq for StringImpl {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl PartialOrd for StringImpl {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StringImpl {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}
