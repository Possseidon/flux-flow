use std::collections::{BTreeMap, BTreeSet};

/// A singular type which [`RuntimeType`]s are eventually made up of.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum MonoType {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    F32,
    F64,
    Char,
    Str,
    Array(StaticType, usize),
    Vec(StaticType),
    Deque(StaticType),
    Set(StaticType),
    Map(StaticType, StaticType),
    Function,
    NativeFunction,
    Generator,
    RawType,
    RuntimeType,
    StaticType,
}

/// A set of distinct types separated by `,` in code.
///
/// `unit` can be used as the identity element.
///
/// # Examples:
///
/// ```flx
/// unit -> {}
/// bool, str -> {bool, str}
/// str, bool -> {bool, str}
/// unit, str -> {str}
///
/// T, T -> error, as types must be distinct
///
/// type X = bool, str;
/// X, i64 -> {bool, str, i64}
/// ```
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct RuntimeTypeSet(pub BTreeSet<MonoType>);

/// A map from names to types.
///
/// # Examples
///
/// ```flx
/// (a: str, b: bool) -> [("a", {str}), ("b", {bool})]
/// (b: bool, a: str) -> [("a", {str}), ("b", {bool})]
/// ```
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct RuntimeTypeStruct(pub BTreeMap<String, RuntimeType>);

/// The runtime type of a value, which is one of many different variants of product types.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum RuntimeType {
    /// An optimization for sets with only a single type.
    Single(MonoType),
    Set(RuntimeTypeSet),
    Struct(RuntimeTypeStruct),
}

impl Default for RuntimeType {
    fn default() -> Self {
        Self::UNIT
    }
}

impl RuntimeType {
    pub const UNIT: RuntimeType = RuntimeType::Set(RuntimeTypeSet(BTreeSet::new()));
}

impl From<RuntimeType> for StaticType {
    fn from(value: RuntimeType) -> Self {
        Self::Set(StaticTypeSet(InfBTreeSet::Union(BTreeSet::from([value]))))
    }
}

/// A set (or complement set) of distinct types separated by `|`, `&` or `-` in code.
///
/// `never` and `any` can be used as the identity element respectively.
///
/// A complement set is denoted using a unary `-`.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StaticTypeSet(pub InfBTreeSet<RuntimeType>);

/// A list of types that are distinguished by their index (which can be omitted).
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StaticTypeList(pub Vec<StaticType>);

/// A map from names to types.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StaticTypeEnum(pub BTreeMap<String, StaticType>);

/// The static type of a variable, which is one of many different kinds of sum types.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum StaticType {
    Set(StaticTypeSet),
    List(StaticTypeList),
    Enum(StaticTypeEnum),
}

impl StaticType {
    // TODO: Simplify once using infset.
    pub const NEVER: Self = Self::Set(StaticTypeSet(InfBTreeSet::Union(BTreeSet::new())));
    pub const ANY: Self = Self::Set(StaticTypeSet(InfBTreeSet::Complement(BTreeSet::new())));

    pub fn contains(&self, _ty: &RuntimeType) -> bool {
        todo!()
    }
}

// TODO: Use infset
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum InfBTreeSet<T> {
    Union(BTreeSet<T>),
    Complement(BTreeSet<T>),
}

impl<T: Default> Default for InfBTreeSet<T> {
    fn default() -> Self {
        Self::Union(Default::default())
    }
}
