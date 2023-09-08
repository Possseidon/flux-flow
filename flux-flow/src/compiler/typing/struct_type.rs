use std::{collections::BTreeSet, sync::Arc};

use super::Type;

/// A set of struct types, uniquely identified and ordered by their field names and types.
///
/// Having the field order be arbitrary would have some benefit, but for the sake of better
/// serialization, the order is part of the type. That is, `(a: .., b: ..)` is a distinct type from
/// `(b: .., a: ..)`.
///
/// Additionally, the type of each field is also part of the ordering. The reason for this is, that
/// `(a: true, b: false) | (a: false, b: true)` would have to get unioned into `(a: bool, b: bool)`.
/// However `(a: bool, b: bool)` would also allow `(a: true, b: true)` and `(a: false, b: false)`
/// which was not part of the original type.
#[derive(Default)]
pub(crate) struct StructTypes {
    structs: Option<Arc<BTreeSet<StructType>>>,
}

struct StructType {
    fields: Arc<[StructField]>,
}

struct StructField {
    name: Arc<str>,
    ty: Arc<Type>,
}
