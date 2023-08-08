pub mod char;
pub mod collections;
pub mod function;
pub mod new_type;
pub mod number;
pub mod string;
pub mod struct_type;
pub mod trait_type;
pub mod uuid;

use std::{
    collections::{BTreeMap, HashMap},
    mem::size_of,
    sync::Arc,
};

use crate::Path;

use self::{
    char::CharType,
    collections::{ListType, MapType, SetType},
    function::FunctionType,
    new_type::NewType,
    number::NumberType,
    string::StringType,
    struct_type::StructType,
    trait_type::TraitType,
    uuid::UuidType,
};

/// The type that a variable has at compile-time.
///
/// It describes the set of possible values that a variable can hold.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    variants: Option<Arc<[TypeVariant]>>,
}

const _: () = assert!(size_of::<Type>() == 16);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeVariant {
    /// A type that can only hold the value `true`.
    True,
    /// A type that can only hold the value `false`.
    False,
    /// A type that can only hold the value `maybe`.
    Maybe,
    /// A type that can hold a certain set of numbers.
    Number(Arc<NumberType>),
    /// A type that can hold a certain set of chars.
    Char(CharType),
    /// A type that can hold a certain set of strings.
    ///
    /// While this conceptually behaves very similar to a list of chars, it is a completely separate
    /// type, mainly because of indexing being `O(n)`.
    String(Arc<StringType>),
    /// A type that can hold a certain set of UUIDs.
    Uuid(Arc<UuidType>),
    /// A type that can hold a time instant.
    ///
    /// Durations do not have their own type and are simply numbers of unit time.
    ///
    /// TODO: Make the following possible:
    ///
    /// The compiler is aware of when an instant of the current time is created, which makes it
    /// possible to know wether the difference between two instants is guaranteed to be zero,
    /// positive or negative.
    Instant,
    /// A list of values that all have the same concrete type.
    ///
    /// This type is used for both fixed size arrays as well as dynamic size vectors. In fact, it is
    /// even more flexible, since the number of elements is expressed as an integer range. In other
    /// words, you can have a type of `[2..=4]bool` which is a list of between 2 and 4 booleans.
    ///
    /// A list might also switch its internal representation to use a deque, e.g. when inserting an
    /// element at the beginning of the list.
    List(Arc<ListType>),
    /// A sorted set of unique values.
    Set(Arc<SetType>),
    /// A sorted map of unique keys to values.
    Map(Arc<MapType>),
    /// A function that simply maps one value to another.
    ///
    /// TODO: Think about side-effects. They should mostly be avoided, but there needs to be some
    /// way to get e.g. the current time or file/network IO, maybe logging, the list goes on.
    Function(Arc<FunctionType>),
    /// A struct, that is, a collection of fields which each have their own type.
    Struct(Arc<StructType>),
    /// Wraps an existing type to create a new distinct type using the same underlying storage.
    NewType(Arc<NewType>),
    /// A type that can hold any value that implements a full set of traits.
    Trait(TraitType),
}

const _: () = assert!(size_of::<TypeVariant>() == 16);

// TODO: This is part of the runtime:

/// Contains implementations for different types.
struct Runtime {
    types: HashMap<Path, Type>,
    type_impls: HashMap<Type, TypeImpl>,
    functions: HashMap<Path, Function>,
}

struct TypeImpl {
    associated_functions: BTreeMap<String, Function>,
    associated_types: BTreeMap<String, Type>,
}
