#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub(super) enum Impl {
    #[default]
    Empty,
}

impl PartialEq<OrderedImpl> for Impl {
    fn eq(&self, other: &OrderedImpl) -> bool {
        todo!()
    }
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub(super) enum OrderedImpl {
    #[default]
    Empty,
}

impl PartialEq<Impl> for OrderedImpl {
    fn eq(&self, other: &Impl) -> bool {
        other == self
    }
}

impl PartialOrd for OrderedImpl {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for OrderedImpl {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        todo!("compare using struct_type")
    }
}
