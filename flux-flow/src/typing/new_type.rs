use super::Type;

use std::sync::Arc;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NewType {
    path: Path,
    ty: Type,
}

// TODO: There are no generics.
//       You can just create functions that spit out other functions/types/traits.
//       Since types can be used as values generic types can just be regular arguments.
