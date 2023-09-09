pub mod collections;
pub mod function;
pub mod meta;
pub mod number;
pub mod string;
pub mod structure;

use std::{
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use enumset::{EnumSet, EnumSetType};
use num::BigRational;

use crate::compiler::typing::{
    number::float::FloatRangeType,
    string::{CharRangeType, StringByLenType},
};

// TODO: use a proper type for this
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Value;

// TODO: Manually implement `Ord` for `Type`, as types are split across different fields
//       They should sort the same as values themselves.
//    -> Both TypeVariant and TypeVariantFlag should already sort correctly within themselves
//       Although I'm not 100% for TypeVariant...
//       Actually, I have no idea how it would even make sense to order e.g. values and ranges.

/// The type of a variable during compile time.
///
/// Flux-Flow comes with a rich set of types for all kinds of common use-cases:
///
/// - Booleans - `true`, `false` as well as an additional `maybe` variant
/// - Numbers - supporting both rationals and floats that can even have units associated with them
/// - Chars and Strings
/// - UUIDs
/// - Instants - points in time using [`Instant`][std::time::Instant]; durations are just numbers
///   associated with the unit "time"
/// - Collections
///   - List - switching between "`[T]`", [`Vec<T>`] and even
///     [`VecDeque<T>`][std::collections::VecDeque] seamlessly
///   - Set - always sorted; uses [`BTreeSet<T>`][std::collections::BTreeSet] as a backing store
///   - Map - always sorted; uses [`BTreeMap<K, V>`][std::collections::BTreeMap] as a backing store
/// - Structs - for both naming values and packing them together
/// - Wrapper-Types - for creating distinct, named types from existing ones
/// - Trait-Bounds - for types that must implement certain functionality through a trait
/// - Functions
/// - Meta-Types - the type of a type
///
/// # Rationals and Floats
///
/// Note that `rational` and `float` are distinct types, which is by design. The reasoning behind
/// this, is that `float` comes with the usual caveats of floating point numbers when it comes to
/// rounding/approximation/precision. A `rational` on the other hand is capable of representing any
/// rational number without any limitations. The only limitation is the operations that can be
/// applied to it. (E.g. trigonometry functions like `sin()` can only be approximated and therefore
/// require the use of `float`).
///
/// # Ordering
///
/// Built-in types have a fixed ordering between them that cannot be changed.
///
/// However, wrapper types have the special property, that they don't have an inherent order and the
/// order is instead defined by each type that uses those wrapper types. But keep in mind, that when
/// mixing wrapped and built-in types, the built-in types will always come first.
///
/// The reasoning behind this intrinsic ordering is that a lot of built-in types have complex rules
/// about how they get unioned. E.g. a type `.., 0 | bool | 0, ..` would be ambiguous in wether `0`
/// is ordered before or after the boolean type. By simply forcing an intrinsic order between
/// boolean and numbers, this type can simply be unioned into `bool | ..`.
///
/// Using wrapped types, `A | B | C` will always order `A` before `B` before `C`. `A | B | A` will
/// ignore the second `A`. Only the first occurence defines the order.
///
/// # Default
///
/// This type does **not** implement the [`Default`] trait. Reason being, any of `never`, `unit` or even
/// `any` could make sense as a default.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Type {
    complement: bool,
    variants: Option<Arc<BTreeSet<TypeVariant>>>,
    variant_flags: EnumSet<TypeVariantFlag>,
    /// Stored separately in an array, since the order of named types is user-defined.
    ///
    /// TODO: Does the order need some special handling when dealing with complement types?
    named_types: Option<Arc<[NamedType]>>,
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Type {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.complement.cmp(&other.complement) {
            Ordering::Equal => {}
            ord => return ord,
        }

        let self_kinds = self.kinds();
        let other_kinds = other.kinds();
        let both_contain = |kind| self_kinds.contains(kind) && other_kinds.contains(kind);

        if both_contain(TypeKind::Boolean) {
            let bool_flags = TypeKind::Boolean.flags();
            match (self.variant_flags & bool_flags).cmp(&(other.variant_flags & bool_flags)) {
                Ordering::Equal => {}
                ord => return ord,
            }
        } else if self_kinds.contains(TypeKind::Boolean) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::Boolean) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::Rational) {
            fn filter_rational(
                variants: &Option<Arc<BTreeSet<TypeVariant>>>,
            ) -> impl Iterator<Item = Option<&Arc<BigRational>>> {
                // Yielding `None` means "there is a range, but it does not have a lower bound"
                // Luckily, `None` is ordered before `Some(..)`, which is exactly what we want here
                variants.iter().flat_map(|variants| {
                    variants.iter().filter_map(|variant| match variant {
                        TypeVariant::Rational(range) => Some(Some(range)),
                        TypeVariant::RationalRange(range) => Some(range.sort_key()),
                        _ => None,
                    })
                })
            }

            match filter_rational(&self.variants).cmp(filter_rational(&other.variants)) {
                Ordering::Equal => {}
                ord => return ord,
            }
        } else if self_kinds.contains(TypeKind::Rational) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::Rational) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::F32) {
            fn filter_f32(
                variants: &Option<Arc<BTreeSet<TypeVariant>>>,
            ) -> impl Iterator<Item = &FloatRangeType<f32>> {
                variants.iter().flat_map(|variants| {
                    variants.iter().filter_map(|variant| match variant {
                        TypeVariant::F32Range(range) => Some(range),
                        _ => None,
                    })
                })
            }

            match filter_f32(&self.variants).cmp(filter_f32(&other.variants)) {
                Ordering::Equal => {}
                ord => return ord,
            }

            // NaN last, similar to how OrderedFloat does it
            let self_has_nan = self.variant_flags.contains(TypeVariantFlag::F32Nan);
            let other_has_nan = other.variant_flags.contains(TypeVariantFlag::F32Nan);
            match self_has_nan.cmp(&other_has_nan) {
                Ordering::Equal => {}
                ord => return ord,
            }
        } else if self_kinds.contains(TypeKind::F32) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::F32) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::F64) {
            fn filter_f64(
                variants: &Option<Arc<BTreeSet<TypeVariant>>>,
            ) -> impl Iterator<Item = &FloatRangeType<f64>> {
                variants.iter().flat_map(|variants| {
                    variants.iter().filter_map(|variant| match variant {
                        TypeVariant::F64Range(range) => Some(range),
                        _ => None,
                    })
                })
            }

            match filter_f64(&self.variants).cmp(filter_f64(&other.variants)) {
                Ordering::Equal => {}
                ord => return ord,
            }

            // NaN last, similar to how OrderedFloat does it
            let self_has_nan = self.variant_flags.contains(TypeVariantFlag::F64Nan);
            let other_has_nan = other.variant_flags.contains(TypeVariantFlag::F64Nan);
            match self_has_nan.cmp(&other_has_nan) {
                Ordering::Equal => {}
                ord => return ord,
            }
        } else if self_kinds.contains(TypeKind::F64) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::F64) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::Char) {
            fn filter_char(
                variants: &Option<Arc<BTreeSet<TypeVariant>>>,
            ) -> impl Iterator<Item = &CharRangeType> {
                variants.iter().flat_map(|variants| {
                    variants.iter().filter_map(|variant| match variant {
                        TypeVariant::CharRange(range) => Some(range),
                        _ => None,
                    })
                })
            }

            match filter_char(&self.variants).cmp(filter_char(&other.variants)) {
                Ordering::Equal => {}
                ord => return ord,
            }
        } else if self_kinds.contains(TypeKind::Char) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::Char) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::String) {
            let self_has_empty_string = &self.variant_flags.contains(TypeVariantFlag::StringEmpty);
            let other_has_empty_string = other.variant_flags.contains(TypeVariantFlag::StringEmpty);
            match self_has_empty_string.cmp(&other_has_empty_string) {
                Ordering::Equal => {}
                ord => return ord,
            }

            fn filter_string_by_len(
                variants: &Option<Arc<BTreeSet<TypeVariant>>>,
            ) -> impl Iterator<Item = &StringByLenType> {
                variants.iter().flat_map(|variants| {
                    variants.iter().filter_map(|variant| match variant {
                        TypeVariant::StringByLen(range) => Some(range),
                        _ => None,
                    })
                })
            }

            match filter_string_by_len(&self.variants).cmp(filter_string_by_len(&other.variants)) {
                Ordering::Equal => {}
                ord => return ord,
            }

            fn filter_string(
                variants: &Option<Arc<BTreeSet<TypeVariant>>>,
            ) -> impl Iterator<Item = &Arc<str>> {
                variants.iter().flat_map(|variants| {
                    variants.iter().filter_map(|variant| match variant {
                        TypeVariant::String(string) => Some(string),
                        _ => None,
                    })
                })
            }

            match filter_string(&self.variants).cmp(filter_string(&other.variants)) {
                Ordering::Equal => {}
                ord => return ord,
            }
        } else if self_kinds.contains(TypeKind::String) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::String) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::Uuid) {
            let uuid_flags = TypeKind::Uuid.flags();
            match (self.variant_flags & uuid_flags).cmp(&(other.variant_flags & uuid_flags)) {
                Ordering::Equal => {}
                ord => return ord,
            }

            fn filter_uuid(
                variants: &Option<Arc<BTreeSet<TypeVariant>>>,
            ) -> impl Iterator<Item = &u128> {
                variants.iter().flat_map(|variants| {
                    variants.iter().filter_map(|variant| match variant {
                        TypeVariant::Uuid(uuid) => Some(uuid),
                        _ => None,
                    })
                })
            }

            match filter_uuid(&self.variants).cmp(filter_uuid(&other.variants)) {
                Ordering::Equal => {}
                ord => return ord,
            }
        } else if self_kinds.contains(TypeKind::Uuid) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::Uuid) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::Instant) {
            // nothing to compare at the moment
        } else if self_kinds.contains(TypeKind::Instant) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::Instant) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::List) {
            todo!();
        } else if self_kinds.contains(TypeKind::List) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::List) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::Set) {
            todo!();
        } else if self_kinds.contains(TypeKind::Set) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::Set) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::Map) {
            todo!();
        } else if self_kinds.contains(TypeKind::Map) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::Map) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::Struct) {
            todo!();
        } else if self_kinds.contains(TypeKind::Struct) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::Struct) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::Function) {
            todo!();
        } else if self_kinds.contains(TypeKind::Function) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::Function) {
            return Ordering::Less;
        }

        if both_contain(TypeKind::Meta) {
            todo!();
        } else if self_kinds.contains(TypeKind::Meta) {
            return Ordering::Greater;
        } else if other_kinds.contains(TypeKind::Meta) {
            return Ordering::Less;
        }

        self.named_types.cmp(&other.named_types)
    }
}

impl Type {
    pub const NEVER: Self = Self {
        variants: None,
        variant_flags: EnumSet::EMPTY,
        named_types: None,
        complement: false,
    };

    pub const ANY: Self = Self {
        variants: None,
        variant_flags: EnumSet::EMPTY,
        named_types: None,
        complement: true,
    };

    fn kinds(&self) -> EnumSet<TypeKind> {
        let flag_kinds = self.variant_flags.iter().map(|flag| flag.kind());
        if let Some(variants) = &self.variants {
            flag_kinds
                .chain(variants.iter().map(|variant| variant.kind()))
                .collect()
        } else {
            flag_kinds.collect()
        }
    }
}

#[derive(Debug, EnumSetType, PartialOrd, Ord)]
enum TypeKind {
    Boolean,
    Rational,
    F32,
    F64,
    Char,
    String,
    Uuid,
    Instant,
    List,
    Set,
    Map,
    Struct,
    Function,
    Meta,
}

impl TypeKind {
    fn flags(self) -> EnumSet<TypeVariantFlag> {
        match self {
            TypeKind::Boolean => {
                TypeVariantFlag::BooleanFalse
                    | TypeVariantFlag::BooleanMaybe
                    | TypeVariantFlag::BooleanTrue
            }
            TypeKind::Rational => EnumSet::EMPTY,
            TypeKind::F32 => TypeVariantFlag::F32Nan.into(),
            TypeKind::F64 => TypeVariantFlag::F64Nan.into(),
            TypeKind::Char => EnumSet::EMPTY,
            TypeKind::String => TypeVariantFlag::StringEmpty.into(),
            TypeKind::Uuid => {
                TypeVariantFlag::UuidNil
                    | TypeVariantFlag::UuidMac
                    | TypeVariantFlag::UuidDce
                    | TypeVariantFlag::UuidMd5
                    | TypeVariantFlag::UuidRandom
                    | TypeVariantFlag::UuidSha1
                    | TypeVariantFlag::UuidSortMac
                    | TypeVariantFlag::UuidSortRand
                    | TypeVariantFlag::UuidCustom
                    | TypeVariantFlag::UuidMax
            }
            TypeKind::Instant => TypeVariantFlag::Instant.into(),
            TypeKind::List => TypeVariantFlag::ListEmpty.into(),
            TypeKind::Set => TypeVariantFlag::SetEmpty.into(),
            TypeKind::Map => TypeVariantFlag::MapEmpty.into(),
            TypeKind::Struct => TypeVariantFlag::StructEmpty.into(),
            TypeKind::Function => EnumSet::EMPTY,
            TypeKind::Meta => EnumSet::EMPTY,
        }
    }
}

#[derive(Debug, EnumSetType, PartialOrd, Ord)]
enum TypeVariantFlag {
    BooleanFalse,
    BooleanMaybe,
    BooleanTrue,
    F32Nan,
    F64Nan,
    StringEmpty,
    UuidNil,
    UuidMac,
    UuidDce,
    UuidMd5,
    UuidRandom,
    UuidSha1,
    UuidSortMac,
    UuidSortRand,
    UuidCustom,
    UuidMax,
    Instant,
    ListEmpty,
    SetEmpty,
    MapEmpty,
    StructEmpty,
    MetaNever,
    MetaAny,
}

impl TypeVariantFlag {
    fn kind(self) -> TypeKind {
        match self {
            Self::BooleanFalse | Self::BooleanMaybe | Self::BooleanTrue => TypeKind::Boolean,
            Self::F32Nan => TypeKind::F32,
            Self::F64Nan => TypeKind::F64,
            Self::StringEmpty => TypeKind::String,
            Self::UuidNil
            | Self::UuidMac
            | Self::UuidDce
            | Self::UuidMd5
            | Self::UuidRandom
            | Self::UuidSha1
            | Self::UuidSortMac
            | Self::UuidSortRand
            | Self::UuidCustom
            | Self::UuidMax => TypeKind::Uuid,
            Self::Instant => TypeKind::Instant,
            Self::ListEmpty => TypeKind::List,
            Self::SetEmpty => TypeKind::Set,
            Self::MapEmpty => TypeKind::Map,
            Self::StructEmpty => TypeKind::Struct,
            Self::MetaNever | Self::MetaAny => TypeKind::Meta,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum TypeVariant {
    RationalRange(Arc<number::rational::RationalRangeType>),
    Rational(Arc<BigRational>),
    F32Range(number::float::FloatRangeType<f32>),
    F64Range(number::float::FloatRangeType<f64>),
    CharRange(string::CharRangeType),
    StringByLen(string::StringByLenType),
    String(Arc<str>),
    Uuid(u128),
    ListByLen(Arc<collections::list::ListByLenType>),
    List(Arc<[Value]>),
    SetByLen(Arc<collections::set::SetByLenType>),
    Set(Arc<BTreeSet<Value>>),
    MapByLen(Arc<collections::map::MapByLenType>),
    Map(Arc<BTreeMap<Value, Value>>),
    Struct(structure::StructType),
    Function(Arc<function::FunctionType>),
    Meta(Type),
}

impl TypeVariant {
    fn kind(&self) -> TypeKind {
        match self {
            Self::Rational(_) => TypeKind::Rational,
            Self::RationalRange(_) => TypeKind::Rational,
            Self::F32Range(_) => TypeKind::F32,
            Self::F64Range(_) => TypeKind::F64,
            Self::CharRange(_) => TypeKind::Char,
            Self::String(_) => TypeKind::String,
            Self::StringByLen(_) => TypeKind::String,
            Self::Uuid(_) => TypeKind::Uuid,
            Self::List(_) => TypeKind::List,
            Self::ListByLen(_) => TypeKind::List,
            Self::Set(_) => TypeKind::Set,
            Self::SetByLen(_) => TypeKind::Set,
            Self::Map(_) => TypeKind::Map,
            Self::MapByLen(_) => TypeKind::Map,
            Self::Struct(_) => TypeKind::Struct,
            Self::Function(_) => TypeKind::Function,
            Self::Meta(_) => TypeKind::Meta,
        }
    }
}

/// Named types are one of `wrapper`, `trait-bounds` or `unit` and identified by index here.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum NamedType {
    Wrapper(usize),
    TraitBounds(Arc<[usize]>),
    Unit(usize),
}
