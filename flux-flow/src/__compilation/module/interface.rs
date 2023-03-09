use std::collections::{BTreeMap, BTreeSet};

use super::{FunctionSignature, Identifier, ResolvedType, Type, TypeDefinition, UseItem};

/// The public interface of a module.
///
/// As long a this does not change, the module stays compatible with its users, and only internal
/// changes cause a patch version increment.
///
/// If the existing interface is still a subset of the old version, i.e. only new stuff has been
/// added, a minor version increment is necessary.
///
/// Otherwise a major version increment is required and the module will no longer be compatible with
/// older versions.
pub struct Interface {
    sub_modules: BTreeMap<Identifier, Interface>,
    uses: BTreeMap<Identifier, UseItem>,
    type_definitions: BTreeMap<Identifier, TypeDefinition>,
    functions: BTreeMap<Identifier, FunctionSignature>,
    constant: BTreeMap<Identifier, ResolvedType>,
    type_implementations: BTreeSet<(Type, BTreeMap<Identifier, FunctionSignature>)>,
    trait_implementations: BTreeSet<(Type, Type, BTreeMap<Identifier, FunctionSignature>)>,
}

impl Interface {
    pub fn is_compatible_with(&self, other: &Self) -> bool {
        todo!()
    }
}
