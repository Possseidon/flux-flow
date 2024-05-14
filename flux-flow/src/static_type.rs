use std::{
    cmp::Reverse,
    collections::{BTreeMap, BTreeSet},
    convert::identity,
    num::{NonZeroU64, NonZeroU8},
    sync::Arc,
};

use enumset::{enum_set, EnumSet, EnumSetType};
use malachite::{Natural, Rational};
use ordered_float::NotNan;

// TODO: I want a function that:
//       Given a static type returns a stack-push/get/... function for a concrete type T.
//       This function can then be used directly in the compiled instructions without having to
//       store the type information to find it out again.

// TODO: Split the different constraints into multiple files

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StaticType {
    flags: EnumSet<TypeFlag>,
    constraints: Option<Arc<Vec<TypeConstraint>>>,
}

impl StaticType {
    /// A type that cannot hold any value.
    ///
    /// Useful for diverging expressions that will never lead to a value.
    pub const NEVER: Self = Self::from_flags(TypeFlag::NEVER);

    /// The unit type, aka the empty struct.
    ///
    /// Often times used, even by the language itself, when a value is required but the concrete
    /// value of it does not matter. E.g. functions that technically don't return anything can (and
    /// will) return this instead.
    ///
    /// This is a distinct concept from "unit values", which associate a unit such as meters to a
    /// value.
    pub const UNIT: Self = Self::from_flags(TypeFlag::UNIT);

    /// A type that can hold any value.
    ///
    /// Useful for true dynamic typing if absolutely nothing about a variable is known.
    pub const ANY: Self = Self::from_flags(TypeFlag::ANY);

    /// Makes the type optional.
    ///
    /// Optionals are represented as zero-to-one element lists. Additionally, since this is such a
    /// common thing to do, a single level of "optional" is encoded directly into the [`TypeFlag`]s.
    pub fn make_optional(mut self) -> Self {
        if self.flags.insert(TypeFlag::WrapOptional) {
            self
        } else {
            Self {
                flags: enum_set!(TypeFlag::EmptyList | TypeFlag::UnitList),
                constraints: Some(Arc::new(vec![TypeConstraint::ListItemType(self)])),
            }
        }
    }

    /// Returns the inner type of an optional.
    ///
    /// Returns an [`Err`] containing the original type if the type is not optional.
    pub fn unwrap_optional(mut self) -> Result<Self, Self> {
        if self.flags.remove(TypeFlag::WrapOptional) {
            return Ok(self);
        }

        self.unwrap_unoptimized_optional()
    }

    /// Returns a type that allows all values except the ones allowed by this type.
    pub fn complement(mut self) -> Self {
        if self.flags.remove(TypeFlag::WrapOptional) {
            let mut flags = enum_set!(TypeFlag::EmptyList | TypeFlag::UnitList);
            if self.flags.remove(TypeFlag::Complement) {
                flags |= TypeFlag::Complement;
            }
            Self {
                flags,
                constraints: Some(Arc::new(vec![TypeConstraint::ListItemType(self)])),
            }
        } else {
            self.flags ^= TypeFlag::Complement;
            self.optimize_optional()
        }
    }

    /// Returns a type that only allows values that are allowed by both types.
    pub fn intersection(self, other: Self) -> Self {
        todo!()
    }

    /// Returns a type that allows values that are allowed by either of the two types.
    pub fn union(self, other: Self) -> Self {
        todo!()
    }

    /// Returns a type that allows all values of `self` except those allowed by `other`.
    pub fn difference(self, other: Self) -> Self {
        todo!()
    }

    /// Returns a type that allows values that are allowed by either of the two types but not both.
    pub fn symmetric_difference(self, other: Self) -> Self {
        todo!()
    }

    /// Constructs from the given flags without any constraints.
    const fn from_flags(flags: EnumSet<TypeFlag>) -> Self {
        Self {
            flags,
            constraints: None,
        }
    }

    /// Unwraps the top-level optional, assuming it is not optimized.
    ///
    /// Panics if it is optimized, i.e. if [`TypeFlag::WrapOptional`] is set.
    ///
    /// Returns an [`Err`] containing the original type if the type is not optional.
    fn unwrap_unoptimized_optional(self) -> Result<Self, Self> {
        assert!(!self.flags.contains(TypeFlag::WrapOptional));

        if self.flags != enum_set!(TypeFlag::EmptyList | TypeFlag::UnitList) {
            return Err(self);
        }

        if let [TypeConstraint::ListItemType(_)] = self.constraints() {
            let TypeConstraint::ListItemType(unwrapped) = self.into_constraints().pop().unwrap()
            else {
                unreachable!();
            };

            Ok(unwrapped)
        } else {
            Err(self)
        }
    }

    /// Optimizes the top-level optional, assuming it is not optmized already.
    ///
    /// Panics if it is optimized, i.e. if [`TypeFlag::WrapOptional`] is set.
    ///
    /// If the type is not an optional, it is returned as is.
    fn optimize_optional(self) -> Self {
        self.unwrap_unoptimized_optional()
            .map_or_else(identity, |unwrapped| unwrapped.make_optional())
    }

    /// Returns a slice of all constraints.
    fn constraints(&self) -> &[TypeConstraint] {
        self.constraints
            .as_ref()
            .map_or(&[][..], |constraints| &constraints[..])
    }

    /// Returns a mutable slice of all constraints.
    fn constraints_mut(&mut self) -> &mut [TypeConstraint] {
        self.constraints
            .as_mut()
            .map_or(
                &mut [][..],
                |constraints| &mut Arc::make_mut(constraints)[..],
            )
    }

    /// Consumes `self` and returns its owned [`TypeConstraint`]s.
    ///
    /// This might still have to clone the [`Vec`], since it is behind an [`Arc`].
    fn into_constraints(self) -> Vec<TypeConstraint> {
        self.constraints.map_or_else(Vec::new, Arc::unwrap_or_clone)
    }
}

/// A series of flags that describe what values a type can hold.
///
/// Additional [`TypeConstraint`]s may apply to some variants.
#[derive(Debug, PartialOrd, Ord, EnumSetType)]
enum TypeFlag {
    /// Marks the type as holding anything except the values that it would hold without this flag.
    ///
    /// If [`TypeFlag::WrapOptional`] is also set, it results in an optional any, since that is more
    /// common than a type that can hold everything except a specific optional.
    ///
    /// This is one of the special flags that don't directly correspond to actual values.
    Complement,

    /// Wraps the original type in a single element list.
    ///
    /// Used as an optimization to avoid going through [`TypeConstraint`].
    ///
    /// If [`TypeFlag::Complement`] is also set, it results in an optional any, since that is more
    /// common than a type that can hold everything except a specific optional.
    ///
    /// When optionals are nested multiple layers deep, the innermost optional is wrapped like this.
    /// Additionally, every other layer also has to be wrapped in this way. This simplifies both
    /// wrapping a type in an option as well as unwrapping that type again. If the outermost
    /// optional was wrapped instead, (un)wrapping would have to recurse through all layers to
    /// update them.
    ///
    /// This is one of the special flags that don't directly correspond to actual values.
    WrapOptional,

    /// The [`bool`] value `false`.
    False,
    /// The value [`Maybe`].
    Maybe,
    /// The [`bool`] value `true`.
    True,

    /// The integer `0`.
    Zero,
    /// The integer `-1`.
    NegOne,
    /// The integer `1`.
    PosOne,
    /// Any negative integer, excluding zero.
    NegInteger,
    /// Any positive integer, excluding zero.
    PosInteger,
    /// Any negative, non-integer rational number.
    NegRational,
    /// Any positive, non-integer rational number.
    PosRational,

    /// [`f32::NEG_INFINITY`].
    NegInfinityF32,
    /// Any negative, non-zero, finite [`f32`].
    NegFiniteF32,
    /// The [`f32`] negative zero.
    NegZeroF32,
    /// The [`f32`] positive zero.
    PosZeroF32,
    /// Any positive, non-zero, finite [`f32`].
    PosFiniteF32,
    /// [`f32::INFINITY`].
    PosInfinityF32,
    /// [`f32::NAN`].
    NaNF32,

    /// [`f64::NEG_INFINITY`].
    NegInfinityF64,
    /// Any negative, non-zero, finite [`f64`].
    NegFiniteF64,
    /// The [`f64`] negative zero.
    NegZeroF64,
    /// The [`f64`] positive zero.
    PosZeroF64,
    /// Any negative, non-zero, finite [`f64`].
    PosFiniteF64,
    /// [`f64::INFINITY`].
    PosInfinityF64,
    /// [`f64::NAN`].
    NaNF64,

    /// The [`char`] `'\0'`.
    NullChar,
    /// Any non-null ASCII char.
    AsciiChar,
    /// Any non-ASCII char.
    UnicodeChar,

    /// The empty [`String`] `""`.
    EmptyString,
    /// Any string containing exactly one [`char`].
    UnitString,
    /// Any string containing at least two [`char`]s.
    String,

    /// Any value with a unit.
    UnitValue,

    /// Any instant in time.
    Instant,

    /// The nil UUID; all zeros.
    NilUuid,
    /// Any UUID except nil and max.
    Uuid,
    /// The max UUID; all ones.
    MaxUuid,

    /// The empty list.
    EmptyList,
    /// Any list with exactly one element.
    UnitList,
    /// Any list with at least two elements.
    List,

    /// The empty set.
    EmptySet,
    /// Any set with exactly one element.
    UnitSet,
    /// Any set with at least two elements.
    ///
    /// Sets contain a set of unique elements and are always sorted.
    Set,

    /// The empty map.
    EmptyMap,
    /// Any map with exactly one element.
    UnitMap,
    /// Any map with at least two elements.
    ///
    /// Maps contain a set of unqiue keys that are always sorted mapping to associated values.
    Map,

    /// The empty struct `()`, also known as "unit".
    EmptyStruct,
    /// Any non-empty struct, e.g. `(foo: ())`.
    Struct,

    /// Any function with any argument and any return value.
    Function,

    /// Allows for distinct types even if they have the same underlying type.
    ///
    /// Similar to the new-type pattern in Rust, but more ingrained into the language, since every
    /// custom type is essentially a "new-type", whereas in Rust structs and enums themselves are
    /// already distinct types on their own.
    Distinct,

    /// A type that satisfies all trait bounds.
    TraitBound,

    /// Any meta type; i.e. variables that hold type information.
    Meta,
}

impl TypeFlag {
    /// The set of [`TypeFlag`]s that allows no value.
    const NEVER: EnumSet<Self> = EnumSet::EMPTY;
    /// The set of [`TypeFlag`]s that allows only the unit type, aka the empty struct.
    const UNIT: EnumSet<Self> = enum_set!(Self::EmptyStruct);
    /// The set of [`TypeFlag`]s that allows any value.
    const ANY: EnumSet<Self> = enum_set!(Self::Complement);
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum TypeConstraint {
    NegIntegerSmallRange(Reverse<SmallRange>),
    NegIntegerRange(Reverse<Arc<NaturalRange>>),
    NegIntegerRanges(Reverse<Arc<Ranges<NaturalRange>>>),
    PosIntegerSmallRange(SmallRange),
    PosIntegerRange(Arc<NaturalRange>),
    PosIntegerRanges(Arc<Ranges<NaturalRange>>),

    NegRationalSmallRange(Reverse<SmallRange>),
    NegRationalRange(Reverse<Arc<RationalRange>>),
    NegRationalRanges(Reverse<Arc<Ranges<RationalRange>>>),
    PosRationalSmallRange(SmallRange),
    PosRationalRange(Arc<RationalRange>),
    PosRationalRanges(Arc<Ranges<RationalRange>>),

    NegFiniteF32Range(Reverse<FiniteRange<NotNan<f32>>>),
    NegFiniteF32Ranges(Reverse<Arc<Ranges<FiniteRange<NotNan<f32>>>>>),
    PosFiniteF32Range(FiniteRange<NotNan<f32>>),
    PosFiniteF32Ranges(Arc<Ranges<FiniteRange<NotNan<f32>>>>),

    NegFiniteF64Range(Reverse<FiniteRange<NotNan<f64>>>),
    NegFiniteF64Ranges(Reverse<Arc<Ranges<FiniteRange<NotNan<f64>>>>>),
    PosFiniteF64Range(FiniteRange<NotNan<f64>>),
    PosFiniteF64Ranges(Arc<Ranges<FiniteRange<NotNan<f64>>>>),

    UnitValue(UnitValueConstraints),

    AsciiCharRange(AsciiCharRange),
    AsciiCharRanges(Arc<Ranges<AsciiCharRange>>),
    UnicodeCharRange(UnicodeCharRange),
    UnicodeCharRanges(Arc<Ranges<UnicodeCharRange>>),

    StringLenRange(Arc<NaturalRange>),
    StringLenRanges(Arc<Ranges<NaturalRange>>),
    StringCharRange(CharRange),
    StringCharRanges(Arc<Ranges<CharRange>>),

    ListLenRange(Arc<NaturalRange>),
    ListLenRanges(Arc<Ranges<NaturalRange>>),
    ListItemType(StaticType),

    SetLenRange(Arc<NaturalRange>),
    SetLenRanges(Arc<Ranges<NaturalRange>>),
    SetItemType(StaticType),

    MapLenRange(Arc<NaturalRange>),
    MapLenRanges(Arc<Ranges<NaturalRange>>),
    MapItemType(Arc<MapItemType>),
    MapItemTypes(Arc<MapItemTypes>),

    StructDefinition(Arc<StructDefinition>),
    StructDefinitions(Arc<StructDefinitions>),

    FunctionSignature(Arc<FunctionSignature>),
    FunctionSignatures(Arc<FunctionSignatures>),

    Distinct(DistinctConstraints),

    TraitBound(TraitBoundConstraints),

    Meta(Arc<MetaConstraints>),
}

impl TypeConstraint {
    /// A cheap dummy value that can be used with [`replace`].
    const DUMMY: Self = Self::StringCharRange(CharRange {
        min: '\0',
        max: '\0',
    });

    /// Which flag this constraint corresponds to.
    ///
    /// This is _not_ sufficient for sorting type constraints, since some [`TypeFlag`]s can have
    /// multiple different constraints. E.g. lists have separate length and item type constraints.
    fn flag(&self) -> TypeFlag {
        match self {
            Self::NegIntegerSmallRange(_)
            | Self::NegIntegerRange(_)
            | Self::NegIntegerRanges(_) => TypeFlag::NegInteger,
            Self::PosIntegerSmallRange(_)
            | Self::PosIntegerRange(_)
            | Self::PosIntegerRanges(_) => TypeFlag::PosInteger,
            Self::NegRationalSmallRange(_)
            | Self::NegRationalRange(_)
            | Self::NegRationalRanges(_) => TypeFlag::NegRational,
            Self::PosRationalSmallRange(_)
            | Self::PosRationalRange(_)
            | Self::PosRationalRanges(_) => TypeFlag::PosRational,
            Self::NegFiniteF32Range(_) | Self::NegFiniteF32Ranges(_) => TypeFlag::NegFiniteF32,
            Self::PosFiniteF32Range(_) | Self::PosFiniteF32Ranges(_) => TypeFlag::PosFiniteF32,
            Self::NegFiniteF64Range(_) | Self::NegFiniteF64Ranges(_) => TypeFlag::NegFiniteF64,
            Self::PosFiniteF64Range(_) | Self::PosFiniteF64Ranges(_) => TypeFlag::PosFiniteF64,
            Self::UnitValue(_) => TypeFlag::UnitValue,
            Self::AsciiCharRange(_) | Self::AsciiCharRanges(_) => TypeFlag::AsciiChar,
            Self::UnicodeCharRange(_) | Self::UnicodeCharRanges(_) => TypeFlag::UnicodeChar,
            Self::StringLenRange(_)
            | Self::StringLenRanges(_)
            | Self::StringCharRange(_)
            | Self::StringCharRanges(_) => TypeFlag::String,
            Self::ListLenRange(_) | Self::ListLenRanges(_) | Self::ListItemType(_) => {
                TypeFlag::List
            }
            Self::SetLenRange(_) | Self::SetLenRanges(_) | Self::SetItemType(_) => TypeFlag::Set,
            Self::MapLenRange(_)
            | Self::MapLenRanges(_)
            | Self::MapItemType(_)
            | Self::MapItemTypes(_) => TypeFlag::Map,
            Self::StructDefinition(_) | Self::StructDefinitions(_) => TypeFlag::Struct,
            Self::FunctionSignature(_) | Self::FunctionSignatures(_) => TypeFlag::Function,
            Self::Distinct(_) => TypeFlag::Distinct,
            Self::TraitBound(_) => TypeFlag::TraitBound,
            Self::Meta(_) => TypeFlag::Meta,
        }
    }
}

/// A series of multiple non-overlapping ranges.
///
/// When possible, ranges must be merged. Ranges will always be in their most greedy form. I.e.
/// `1, 2, 3, 5, 7` must be represented as `1, 2, 3` followed by `5, 7`.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Ranges<T>(Vec<T>);

enum RangeResult<T> {
    Range(T),
    Ranges(Ranges<T>),
}

impl<T> RangeResult<T> {
    fn map<R>(self, map_range: impl FnOnce(T) -> R, map_ranges: impl FnOnce(Ranges<T>) -> R) -> R {
        match self {
            Self::Range(range) => map_range(range),
            Self::Ranges(ranges) => map_ranges(ranges),
        }
    }
}

/// A potentially unbounded range of small positive integers.
///
/// This is preferred over [`NaturalRange`] when possible.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct SmallRange {
    /// The minimum value in the range.
    ///
    /// At least `2`, as both `0` and `1` are expressed as [`TypeFlag`]s.
    min: NonZeroU64,
    /// The maximum value in the range.
    ///
    /// [`None`] for an unbounded range. Must not be less than `min`.
    max: Option<NonZeroU64>,
}

/// A potentially unbounded range of positive integers.
///
/// Also supports step size for ranges such as `1, 3, 5, 7, ..`
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct NaturalRange {
    /// The minimum value in the range.
    ///
    /// At least `2`, as both `0` and `1` are expressed as [`TypeFlag`]s.
    min: Natural,
    /// The maximum value in the range.
    ///
    /// [`None`] for an unbounded range. Must not be less than `min`.
    max: Option<Natural>,
    /// The step size for valid values.
    ///
    /// If `max` is set, `(max - min) % step` must be `0`.
    ///
    /// Must be `1` if `min` and `max` are equal.
    step: Natural,
}

/// A potentially unbounded range of positive rational numbers.
///
/// Supports both continuous as well as discrete ranges with a fixed step size.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct RationalRange {
    /// The lower bound of valid values in this range.
    ///
    /// Must be [`RationalMin::Inclusive`] if the range is also [`RationalQuantity::Discrete`].
    min: RationalMin,
    /// The upper bound of valid values in this range.
    ///
    /// If either of the two bounds is exclusive, `min` must be less than `max`. Otherwise they can
    /// be equal.
    ///
    /// If the range is [`RationalQuantity::Discrete`], then:
    ///
    /// - `max` must not be [`RationalMax::Exclusive`]
    /// - `max` must fall on a discrete step
    max: RationalMax,
    /// Whether the valid values are continuous or have to fall on discrete steps.
    quantity: RationalQuantity,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum RationalMin {
    /// An inclusive bound for the lower bound of a [`RationalRange`].
    ///
    /// Must be greater than `0`, since [`RationalRange`] only covers non-integer numbers.
    Inclusive(Rational),
    /// An exclusive bound for the lower bound of a [`RationalRange`].
    ///
    /// Must be at least `0`.
    Exclusive(Rational),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum RationalMax {
    /// Indicates an unbounded upper bound of a [`RationalRange`].
    Unbounded,
    /// An inclusive bound for the upper bound of a [`RationalRange`].
    Inclusive(Rational),
    /// An exclusive bound for the upper bound of a [`RationalRange`].
    Exclusive(Rational),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum RationalQuantity {
    /// All values within the range are valid.
    Continuous,
    /// Only values that are `N * step` away from `min` are part of the range.
    ///
    /// E.g. `0.5` could result in the discrete values `0.5, 1.0, 1.5, 2.0, ..`
    Discrete { step: Rational },
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct FiniteRange<T> {
    min: T,
    max: T,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct UnitValueConstraints {
    // TODO
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct AsciiCharRange {
    min: NonZeroU8,
    max: NonZeroU8,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct UnicodeCharRange {
    min: char,
    max: char,
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct CharRange {
    min: char,
    max: char,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct MapItemType {
    key: StaticType,
    value: StaticType,
}

/// Contains a set of map key-value pairs.
///
/// Item types must be merged where possible as long as no information is lost in the process. The
/// same rules as for [`StructDefinitions`] apply. [`MapItemType`] can simply be treated as a struct
/// with two fields for key and value.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct MapItemTypes {
    item_types: BTreeSet<MapItemType>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct StructDefinition {
    /// TODO: SBO String, field names are usually short
    /// TODO: remember insertion order like indexmap but have it backed by a BTreeMap for Ord
    fields: BTreeMap<String, StaticType>,
}

/// Contains a set of struct definitions.
///
/// Definitions must be merged where possible as long as no information is lost in the process.
/// Merging is performed greedily from left to right.
///
/// Merging works as follows:
///
/// - For all `k=2` combinations
/// - Find the first pairs for which:
///   - Have the same fields (by name)
///   - And the field types only differ for a single field.
/// - Merge them together.
/// - Repeat until no more are found.
///
/// E.g. `y` can be merged in the following example:
///
/// ```txt
/// (x: A, y: B,     z: D)
/// (x: A, y:     C, z: D)
/// ----------------------
/// (x: A, y: B | C, z: D)
/// ```
///
/// TODO: Keep in mind that complement sets probably need some inverted logic here.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct StructDefinitions {
    definitions: BTreeSet<StructDefinition>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct FunctionSignature {
    // TODO: other properties like pureness?
    arg: StaticType,
    ret: StaticType,
}

/// Contains a set of function signatures.
///
/// Signatures must be merged where possible as long as no information is lost in the process. The
/// same rules as for [`StructDefinitions`] apply. [`FunctionSignature`] can be treated as a struct
/// with two fields for argument and return value. However one caveat applies: The argument is
/// contravariant and therefore has to use the same inverted logic that is used by complement sets.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct FunctionSignatures {
    signatures: BTreeSet<FunctionSignature>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct DistinctConstraints {
    // TODO
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct TraitBoundConstraints {
    // TODO
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct MetaConstraints {
    types: BTreeSet<StaticType>,
}

/// Can be used if something might be either `true` or `false` but is not currently known.
///
/// Notably `false && maybe == false` and `true || maybe == true`. Most other operations will simply
/// result in `maybe`.
struct Maybe;
