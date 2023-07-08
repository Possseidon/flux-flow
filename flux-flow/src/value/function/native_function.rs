use super::ExecutionEnvironment;

#[derive(Clone, Copy)]
pub(crate) struct NativeFunction(pub(crate) fn(&mut ExecutionEnvironment));

impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NativeFunction")
            .field(&(self.0 as *const ()))
            .finish()
    }
}

impl std::hash::Hash for NativeFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as usize);
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

impl Eq for NativeFunction {}

impl PartialOrd for NativeFunction {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for NativeFunction {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.0 as usize).cmp(&(other.0 as usize))
    }
}
