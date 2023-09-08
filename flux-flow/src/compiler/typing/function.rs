use std::{collections::BTreeSet, sync::Arc};

use super::Type;

/// The type of a set of functions, uniquely identified and ordered by argument and return type.
#[derive(Default)]
pub(crate) struct FunctionTypes {
    functions: Option<Arc<BTreeSet<FunctionType>>>,
}

struct FunctionType {
    arg_type: Type,
    ret_type: Type,
}
