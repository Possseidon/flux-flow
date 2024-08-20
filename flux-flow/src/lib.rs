use std::collections::BTreeMap;

/// A single value, like `42`, `true` or `"hello"`.
///
/// [`Type`]s and [`Trait`]s are also [`Value`]s to allow for meta-programming.
struct Value {
    value: ValueImpl,
}

/// Describes a set of [`Value`]s.
///
/// [`Value`]s themselves are also [`Type`]s, that only hold that single [`Value`].
///
/// Also contains a special `Self` variant, to allow [`Trait`]s to reference associated items.
struct Type {
    kinds: ValueKinds,
    constraints: ValueConstraints,
}

/// Behavior that can be implemented for certain [`Value`]s, usually over entire [`Type`]s.
struct Trait {
    /// Associated items such as functions, types and constants.
    items: Vec<(String, Type)>,
}

/// Associates concrete behavior to a [`Type`].
struct Impl {
    /// The [`Type`] for which to implement the [`Trait`].
    ty: Type,
    /// The values for all associated items.
    ///
    /// These can reference `Self` as well as other associated items via `Self::foo`.
    items: Vec<(String, Value)>,
}

struct Env {
    /// A collection of all [`Impl`] blocks.
    impls: Vec<Impl>,
    /// A collection of all [`Trait`]s as well as all their concrete implementations.
    traits: BTreeMap<Trait, Vec<Impl>>,
}
