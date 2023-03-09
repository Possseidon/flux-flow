pub mod interface;
pub mod interfaceable;

use std::collections::BTreeMap;

use self::interfaceable::Interfaceable;

struct ResolvedType;

pub struct ModuleRoot {
    modules: BTreeMap<Identifier, Module>,
}

/// A single source file with named types, functions, etc...
pub struct Module {
    /// Nested modules using the `mod` keyword.
    // sub_modules: BTreeMap<Identifier, Interfaceable<Module>>,
    /// Uses are resolve using the [`ModuleTree`].
    // uses: BTreeMap<Identifier, Interfaceable<UseItem>>,
    /// All types that are defined by this module.
    // type_definitions: BTreeMap<Identifier, Interfaceable<TypeDefinition>>,
    /// All functions that are defined by this module.
    functions: BTreeMap<Identifier, Interfaceable<Function>>,
    /// All constants that are defined by this module.
    constants: BTreeMap<Identifier, Interfaceable<Constant>>,
}

struct FunctionSignature;

pub struct SubModuleReference;

pub enum Type {
    Path(Path),
}

pub struct TypeDefinition;

pub struct UseItem {
    path: Path,
}

/// A `::` delimited path of identifiers.
pub struct Path {
    root: Identifier,
    sub: Vec<Identifier>,
}

pub struct Identifier(String);
