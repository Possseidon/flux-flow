use std::{
    collections::{BTreeMap, BTreeSet},
    convert::identity,
    num::{NonZeroU64, NonZeroU8},
    sync::Arc,
};

use enumset::{enum_set, EnumSet, EnumSetType};
use itertools::{merge_join_by, EitherOrBoth, Itertools};
use malachite::{Natural, Rational};
use ordered_float::NotNan;

// TODO: I want a function that:
//       Given a static type returns a stack-push/get/... function for a concrete type T.
//       This function can then be used directly in the compiled instructions without having to
//       store the type information to find it out again.

// TODO: Split the different constraints into multiple files

#[derive(Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StaticType {
    flags: EnumSet<TypeFlag>,
    /// Sorted and unique by [`TypeConstraint::kind`].
    constraints: Option<Arc<Vec<TypeConstraint>>>,
}

impl std::fmt::Debug for StaticType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("StaticType")?;
        if self.flags.contains(TypeFlag::WrapOptional) {
            f.write_str("::Optional")?;
        }
        if self.flags.contains(TypeFlag::Complement) {
            f.write_str("::Complement")?;
        }
        f.write_str(" ")?;
        f.debug_map().entries(self.iter_types()).finish()
    }
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
    pub fn optional(mut self) -> Self {
        if self.flags.insert(TypeFlag::WrapOptional) {
            self
        } else {
            Self {
                flags: enum_set!(TypeFlag::EmptyList | TypeFlag::UnitList),
                constraints: Some(Arc::new(vec![TypeConstraint::UnitListItemType(self)])),
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
            Self {
                flags: enum_set!(TypeFlag::Complement | TypeFlag::EmptyList | TypeFlag::UnitList),
                constraints: Some(Arc::new(vec![TypeConstraint::UnitListItemType(self)])),
            }
        } else {
            self.flags ^= TypeFlag::Complement;
            self.optimize_optional()
        }
    }

    /// Returns whether this type allows at least all values that `other` allows.
    pub fn is_superset(self, other: Self) -> bool {
        todo!()
    }

    /// Returns whether this type allows at most all values that `other` allows.
    pub fn is_subset(self, other: Self) -> bool {
        other.is_superset(self)
    }

    /// Returns a type that allows values that are allowed by either of the two types.
    pub fn union(self, other: Self) -> Self {
        self.complement()
            .intersection(other.complement())
            .complement()
    }

    /// Returns a type that only allows values that are allowed by both types.
    pub fn intersection(self, other: Self) -> Self {
        let lhs = self.unoptimize_optional();
        let rhs = other.unoptimize_optional();
        match (
            lhs.flags.contains(TypeFlag::Complement),
            rhs.flags.contains(TypeFlag::Complement),
        ) {
            (true, true) => lhs.complement().raw_union(rhs.complement()),
            (true, false) => rhs.raw_difference(lhs.complement()),
            (false, true) => lhs.raw_difference(rhs.complement()),
            (false, false) => lhs.raw_intersection(rhs),
        }
        .optimize_optional()
    }

    /// Returns a type that allows the same values except those in `other`.
    pub fn difference(self, other: Self) -> Self {
        self.intersection(other.complement())
    }

    /// Returns a type that allows values that are allowed by either of the two types but not both.
    pub fn symmetric_difference(self, other: Self) -> Self {
        self.intersection(other).complement()
    }
}

impl StaticType {
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

        if let [TypeConstraint::UnitListItemType(_)] = self.constraints() {
            let TypeConstraint::UnitListItemType(unwrapped) =
                self.into_constraints().pop().unwrap()
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
            .map_or_else(identity, |unwrapped| unwrapped.optional())
    }

    /// If [`TypeFlag::WrapOptional`] is set, clears it and replaces it with its unoptimized form.
    fn unoptimize_optional(mut self) -> Self {
        if self.flags.remove(TypeFlag::WrapOptional) {
            Self {
                flags: enum_set!(TypeFlag::EmptyList | TypeFlag::UnitList),
                constraints: Some(Arc::new(vec![TypeConstraint::UnitListItemType(self)])),
            }
        } else {
            self
        }
    }

    /// Returns an iterator over all [`TypeFlag`]s together with their [`TypeConstraint`]s.
    ///
    /// [`TypeFlag::WrapOptional`] and [`TypeFlag::Complement`] are separated out beforehand.
    fn iter_types(&self) -> impl Iterator<Item = (TypeFlag, &[TypeConstraint])> {
        self.flags
            .difference(TypeFlag::WrapOptional | TypeFlag::Complement)
            .into_iter()
            .map({
                let mut remaining_constraints = self.constraints();
                move |flag| {
                    let next_flag_type = remaining_constraints
                        .iter()
                        .position(|constraint| constraint.flag() != flag)
                        .unwrap_or(remaining_constraints.len());
                    let (matching_constraints, rest) =
                        remaining_constraints.split_at(next_flag_type);
                    remaining_constraints = rest;
                    (flag, matching_constraints)
                }
            })
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

    fn filter_constraints(self, flags: EnumSet<TypeFlag>) -> impl Iterator<Item = TypeConstraint> {
        self.into_constraints()
            .into_iter()
            .filter(move |constraint| flags.contains(constraint.flag()))
    }

    /// Returns the union of two [raw](Self::is_raw) [`StaticType`]s.
    ///
    /// Panics if one of the types is not raw.
    fn raw_union(self, other: Self) -> Self {
        self.raw_merge_into_superset(other, TypeConstraint::union)
    }

    /// Returns the intersection of two [raw](Self::is_raw) [`StaticType`]s.
    ///
    /// Panics if one of the types is not raw.
    fn raw_intersection(self, other: Self) -> Self {
        self.raw_merge_into_subset(other, TypeConstraint::intersection)
    }

    /// Returns the difference of two [raw](Self::is_raw) [`StaticType`]s.
    ///
    /// Panics if one of the types is not raw.
    fn raw_difference(self, other: Self) -> Self {
        self.raw_merge_into_subset(other, TypeConstraint::difference)
    }

    /// Merges two [raw](Self::is_raw) [`StaticType`]s into a superset.
    ///
    /// `merge` should be [`TypeConstraint::union`].
    ///
    /// Panics if one of the types is not raw.
    fn raw_merge_into_superset(
        self,
        other: Self,
        merge: impl Fn(TypeConstraint, TypeConstraint) -> Option<TypeConstraint>,
    ) -> Self {
        assert!(self.is_raw());
        assert!(other.is_raw());

        let flags = self.flags.intersection(other.flags);
        Self::with_constraints(
            flags,
            merge_join_by(
                self.filter_constraints(flags),
                other.filter_constraints(flags),
                |left, right| left.kind().cmp(&right.kind()),
            )
            .flat_map(|either_or_both| match either_or_both {
                EitherOrBoth::Both(left, right) => merge(left, right),
                EitherOrBoth::Left(constraint) | EitherOrBoth::Right(constraint) => {
                    Some(constraint)
                }
            })
            .collect_vec(),
        )
    }

    /// Merges two [raw](Self::is_raw) [`StaticType`]s into a subset.
    ///
    /// `merge` should be [`TypeConstraint::intersection`] or [`TypeConstraint::difference`].
    ///
    /// Panics if one of the types is not raw.
    fn raw_merge_into_subset(
        self,
        other: Self,
        merge: impl Fn(TypeConstraint, TypeConstraint) -> Option<TypeConstraint>,
    ) -> Self {
        assert!(self.is_raw());
        assert!(other.is_raw());

        let mut flags = self.flags.intersection(other.flags);

        // more complicated than raw_merge_into_superset since flags might need to get cleared
        let constraints = merge_join_by(
            self.filter_constraints(flags),
            other.filter_constraints(flags),
            |left, right| left.kind().cmp(&right.kind()),
        )
        .flat_map(|either_or_both| match either_or_both {
            EitherOrBoth::Both(left, right) => {
                let flag = left.flag();
                assert!(right.flag() == flag);

                let intersection = merge(left, right);
                if intersection.is_none() {
                    flags.remove(flag);
                }
                intersection
            }
            EitherOrBoth::Left(constraint) | EitherOrBoth::Right(constraint) => Some(constraint),
        })
        .collect_vec();

        Self::with_constraints(flags, constraints)
    }

    /// Whether this type has neither [`TypeFlag::WrapOptional`] nor [`TypeFlag::Complement`] set.
    fn is_raw(&self) -> bool {
        !self.flags.contains(TypeFlag::WrapOptional) && !self.flags.contains(TypeFlag::Complement)
    }

    /// Constructs a new [`StaticType`] from the given flags and constraints [`Vec`].
    ///
    /// If the constraints [`Vec`] is empty, it is correctly stored as just [`None`].
    fn with_constraints(flags: EnumSet<TypeFlag>, constraints: Vec<TypeConstraint>) -> Self {
        Self {
            flags,
            constraints: (!constraints.is_empty()).then(|| Arc::new(constraints)),
        }
    }
}

/// A series of flags that describe what values a type can hold.
///
/// Additional [`TypeConstraint`]s may apply to some variants.
#[derive(Debug, PartialOrd, Ord, EnumSetType)]
enum TypeFlag {
    /// Wraps the original type in a zero-to-one element list.
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

    /// Marks the type as holding anything except the values that it would hold without this flag.
    ///
    /// If [`TypeFlag::WrapOptional`] is also set, it results in an optional any, since that is more
    /// common than a type that can hold everything except a specific optional.
    ///
    /// This is one of the special flags that don't directly correspond to actual values.
    Complement,

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
    NegIntegerSmallRange(SmallRange),
    NegIntegerRange(Arc<NaturalRange>),
    NegIntegerRanges(Arc<Ranges<NaturalRange>>),
    PosIntegerSmallRange(SmallRange),
    PosIntegerRange(Arc<NaturalRange>),
    PosIntegerRanges(Arc<Ranges<NaturalRange>>),

    NegRationalSmallRange(SmallRange),
    NegRationalRange(Arc<RationalRange>),
    NegRationalRanges(Arc<Ranges<RationalRange>>),
    PosRationalSmallRange(SmallRange),
    PosRationalRange(Arc<RationalRange>),
    PosRationalRanges(Arc<Ranges<RationalRange>>),

    NegFiniteF32Range(FiniteRange<NotNan<f32>>),
    NegFiniteF32Ranges(Arc<Ranges<FiniteRange<NotNan<f32>>>>),
    PosFiniteF32Range(FiniteRange<NotNan<f32>>),
    PosFiniteF32Ranges(Arc<Ranges<FiniteRange<NotNan<f32>>>>),

    NegFiniteF64Range(FiniteRange<NotNan<f64>>),
    NegFiniteF64Ranges(Arc<Ranges<FiniteRange<NotNan<f64>>>>),
    PosFiniteF64Range(FiniteRange<NotNan<f64>>),
    PosFiniteF64Ranges(Arc<Ranges<FiniteRange<NotNan<f64>>>>),

    UnitValue(UnitValueConstraints),

    AsciiCharRange(AsciiCharRange),
    AsciiCharRanges(Arc<Ranges<AsciiCharRange>>),
    UnicodeCharRange(UnicodeCharRange),
    UnicodeCharRanges(Arc<Ranges<UnicodeCharRange>>),

    UnitStringCharRange(CharRange),
    UnitStringCharRanges(Arc<Ranges<CharRange>>),
    StringLenSmallRange(SmallRange),
    StringLenRange(Arc<NaturalRange>),
    StringLenRanges(Arc<Ranges<NaturalRange>>),
    StringCharRange(CharRange),
    StringCharRanges(Arc<Ranges<CharRange>>),

    UnitListItemType(StaticType),
    UnitListItemTypes(Arc<BTreeSet<StaticType>>),
    ListLenSmallRange(SmallRange),
    ListLenRange(Arc<NaturalRange>),
    ListLenRanges(Arc<Ranges<NaturalRange>>),
    ListItemType(StaticType),
    ListItemTypes(Arc<BTreeSet<StaticType>>),

    UnitSetItemType(StaticType),
    UnitSetItemTypes(Arc<BTreeSet<StaticType>>),
    SetLenSmallRange(SmallRange),
    SetLenRange(Arc<NaturalRange>),
    SetLenRanges(Arc<Ranges<NaturalRange>>),
    SetItemType(StaticType),
    SetItemTypes(Arc<BTreeSet<StaticType>>),

    UnitMapItemType(Arc<MapItemType>),
    UnitMapItemTypes(Arc<MapItemTypes>),
    MapLenSmallRange(SmallRange),
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
    // /// A cheap dummy value that can be used with [`replace`].
    // const DUMMY: Self = Self::StringCharRange(CharRange {
    //     min: '\0',
    //     max: '\0',
    // });

    /// Which flag this constraint corresponds to.
    fn flag(&self) -> TypeFlag {
        self.kind().flag()
    }

    /// The kind of this constraint.
    fn kind(&self) -> TypeConstraintKind {
        match self {
            TypeConstraint::NegIntegerSmallRange(_)
            | TypeConstraint::NegIntegerRange(_)
            | TypeConstraint::NegIntegerRanges(_) => TypeConstraintKind::NegInteger,
            TypeConstraint::PosIntegerSmallRange(_)
            | TypeConstraint::PosIntegerRange(_)
            | TypeConstraint::PosIntegerRanges(_) => TypeConstraintKind::PosInteger,
            TypeConstraint::NegRationalSmallRange(_)
            | TypeConstraint::NegRationalRange(_)
            | TypeConstraint::NegRationalRanges(_) => TypeConstraintKind::NegRational,
            TypeConstraint::PosRationalSmallRange(_)
            | TypeConstraint::PosRationalRange(_)
            | TypeConstraint::PosRationalRanges(_) => TypeConstraintKind::PosRational,
            TypeConstraint::NegFiniteF32Range(_) | TypeConstraint::NegFiniteF32Ranges(_) => {
                TypeConstraintKind::NegFiniteF32
            }
            TypeConstraint::PosFiniteF32Range(_) | TypeConstraint::PosFiniteF32Ranges(_) => {
                TypeConstraintKind::PosFiniteF32
            }
            TypeConstraint::NegFiniteF64Range(_) | TypeConstraint::NegFiniteF64Ranges(_) => {
                TypeConstraintKind::NegFiniteF64
            }
            TypeConstraint::PosFiniteF64Range(_) | TypeConstraint::PosFiniteF64Ranges(_) => {
                TypeConstraintKind::PosFiniteF64
            }
            TypeConstraint::UnitValue(_) => TypeConstraintKind::UnitValue,
            TypeConstraint::AsciiCharRange(_) | TypeConstraint::AsciiCharRanges(_) => {
                TypeConstraintKind::AsciiChar
            }
            TypeConstraint::UnicodeCharRange(_) | TypeConstraint::UnicodeCharRanges(_) => {
                TypeConstraintKind::UnicodeChar
            }
            TypeConstraint::UnitStringCharRange(_) | TypeConstraint::UnitStringCharRanges(_) => {
                TypeConstraintKind::UnitStringChar
            }
            TypeConstraint::StringLenSmallRange(_)
            | TypeConstraint::StringLenRange(_)
            | TypeConstraint::StringLenRanges(_) => TypeConstraintKind::StringLen,
            TypeConstraint::StringCharRange(_) | TypeConstraint::StringCharRanges(_) => {
                TypeConstraintKind::StringChar
            }
            TypeConstraint::UnitListItemType(_) | TypeConstraint::UnitListItemTypes(_) => {
                TypeConstraintKind::UnitListItemType
            }
            TypeConstraint::ListLenSmallRange(_)
            | TypeConstraint::ListLenRange(_)
            | TypeConstraint::ListLenRanges(_) => TypeConstraintKind::ListLen,
            TypeConstraint::ListItemType(_) | TypeConstraint::ListItemTypes(_) => {
                TypeConstraintKind::ListItemType
            }
            TypeConstraint::UnitSetItemType(_) | TypeConstraint::UnitSetItemTypes(_) => {
                TypeConstraintKind::UnitSetItemType
            }
            TypeConstraint::SetLenSmallRange(_)
            | TypeConstraint::SetLenRange(_)
            | TypeConstraint::SetLenRanges(_) => TypeConstraintKind::SetLen,
            TypeConstraint::SetItemType(_) | TypeConstraint::SetItemTypes(_) => {
                TypeConstraintKind::SetItemType
            }
            TypeConstraint::UnitMapItemType(_) | TypeConstraint::UnitMapItemTypes(_) => {
                TypeConstraintKind::UnitMapItemType
            }
            TypeConstraint::MapLenSmallRange(_)
            | TypeConstraint::MapLenRange(_)
            | TypeConstraint::MapLenRanges(_) => TypeConstraintKind::MapLen,
            TypeConstraint::MapItemType(_) | TypeConstraint::MapItemTypes(_) => {
                TypeConstraintKind::MapItemType
            }
            TypeConstraint::StructDefinition(_) | TypeConstraint::StructDefinitions(_) => {
                TypeConstraintKind::StructDefinition
            }
            TypeConstraint::FunctionSignature(_) | TypeConstraint::FunctionSignatures(_) => {
                TypeConstraintKind::FunctionSignature
            }
            TypeConstraint::Distinct(_) => TypeConstraintKind::Distinct,
            TypeConstraint::TraitBound(_) => TypeConstraintKind::TraitBound,
            TypeConstraint::Meta(_) => TypeConstraintKind::Meta,
        }
    }

    /// Returns a [`TypeConstraint`] that allows values that are allowed by either of the two.
    ///
    /// Returns [`None`] if the union is no longer constraining any value.
    ///
    /// Panics if the two types have differing [`TypeConstraintKind`]s.
    fn union(self, other: Self) -> Option<Self> {
        self.merge::<Union>(other)
    }

    /// Returns a [`TypeConstraint`] that only allows values that are allowed by both.
    ///
    /// Returns [`None`] if the intersection is empty.
    ///
    /// Panics if the two types have differing [`TypeConstraintKind`]s.
    fn intersection(self, other: Self) -> Option<Self> {
        self.merge::<Intersection>(other)
    }

    /// Returns a [`TypeConstraint`] that allows the same values except those in `other`.
    ///
    /// Returns [`None`] if the union is no longer constraining any value.
    ///
    /// Panics if the two types have differing [`TypeConstraintKind`]s.
    fn difference(self, other: Self) -> Option<Self> {
        self.merge::<Difference>(other)
    }

    fn merge<T: Merge>(self, other: Self) -> Option<Self> {
        macro_rules! match_all_impl {
            ( $( [ $( $Lhs:ident $( [ $Rhs:ident $merge:ident [ $( $Build:ident )* ]] )* )* ] )* ) => {
                match (self, other) {
                    $( $( $( (Self::$Lhs(lhs), Self::$Rhs(rhs)) => {
                        T::$merge(lhs.into(), rhs.into(), ( $( Self::$Build ),* ) )
                    } )* )* )*
                    _ => panic!("incompatible type constraints"),
                }
            };
        }

        macro_rules! match_all_spread_rhs {
            ( $( [ $( $Lhs:ident [ [ $( $Rhs:ident )* ] $merge:ident $Build:tt ] )* ] )* ) => {
                match_all_impl!( $( [ $( $Lhs $( [ $Rhs $merge $Build ] )* )* ] )* )
            };
        }

        macro_rules! match_all_spread_lhs {
            ( $( [ $( $Lhs:ident )* ] $Rest:tt )* ) => {
                match_all_spread_rhs!( $( [ $( $Lhs $Rest )* ] )* )
            };
        }

        macro_rules! match_all {
            ( $( $( $Constraint:ident )|* => $merge:ident, )* ) => {
                match_all_spread_lhs!( $(
                    [ $( $Constraint )* ]
                    [[ $( $Constraint )* ] $merge [ $( $Constraint )* ]]
                )* )
            };
        }

        match_all! {
            NegIntegerSmallRange | NegIntegerRange | NegIntegerRanges => merge_natural_ranges,
            PosIntegerSmallRange | PosIntegerRange | PosIntegerRanges => merge_natural_ranges,

            // NegRationalSmallRange | NegRationalRange | NegRationalRanges => merge_rational_ranges,
            // PosRationalSmallRange | PosRationalRange | PosRationalRanges => merge_rational_ranges,

            // NegFiniteF32Range | NegFiniteF32Ranges => merge_f32_ranges,
            // PosFiniteF32Range | PosFiniteF32Ranges => merge_f32_ranges,

            // NegFiniteF64Range | NegFiniteF64Ranges => merge_f64_ranges,
            // PosFiniteF64Range | PosFiniteF64Ranges => merge_f64_ranges,

            // UnitValue => merge_unit_values,

            // AsciiCharRange | AsciiCharRanges => merge_ascii_char_ranges,
            // UnicodeCharRange | UnicodeCharRanges => merge_unicode_char_ranges,

            // UnitStringCharRange | UnitStringCharRanges => merge_char_ranges,
            StringLenSmallRange | StringLenRange | StringLenRanges => merge_natural_ranges,
            // StringCharRange | StringCharRanges => merge_char_ranges,

            // UnitListItemType | UnitListItemTypes => merge_types,
            ListLenSmallRange | ListLenRange | ListLenRanges => merge_natural_ranges,
            // ListItemType | ListItemTypes => merge_types,

            // UnitSetItemType | UnitSetItemTypes => merge_types,
            SetLenSmallRange | SetLenRange | SetLenRanges => merge_natural_ranges,
            // SetItemType | SetItemTypes => merge_types,

            // UnitMapItemType | UnitMapItemTypes => merge_map_item_types,
            MapLenSmallRange | MapLenRange | MapLenRanges => merge_natural_ranges,
            // MapItemType | MapItemTypes => merge_map_item_types,

            // StructDefinition | StructDefinitions => merge_struct_definitions,

            // FunctionSignature | FunctionSignatures => merge_function_signatures,

            // Distinct => merge_distincts,

            // TraitBound => merge_trait_bounds,

            // Meta => merge_metas,
        }
    }
}

trait BuildNaturalRange {
    fn small_range(self, range: SmallRange) -> TypeConstraint;
    fn natural_range(self, range: Arc<NaturalRange>) -> TypeConstraint;
    fn natural_ranges(self, range: Arc<Ranges<NaturalRange>>) -> TypeConstraint;
}

impl<A, B, C> BuildNaturalRange for (A, B, C)
where
    A: FnOnce(SmallRange) -> TypeConstraint,
    B: FnOnce(Arc<NaturalRange>) -> TypeConstraint,
    C: FnOnce(Arc<Ranges<NaturalRange>>) -> TypeConstraint,
{
    fn small_range(self, range: SmallRange) -> TypeConstraint {
        self.0(range)
    }

    fn natural_range(self, range: Arc<NaturalRange>) -> TypeConstraint {
        self.1(range)
    }

    fn natural_ranges(self, range: Arc<Ranges<NaturalRange>>) -> TypeConstraint {
        self.2(range)
    }
}

trait Merge {
    fn merge_natural_ranges(
        lhs: AnyNaturalRange,
        rhs: AnyNaturalRange,
        build: impl BuildNaturalRange,
    ) -> Option<TypeConstraint>;
}

struct Union;

impl Merge for Union {
    fn merge_natural_ranges(
        lhs: AnyNaturalRange,
        rhs: AnyNaturalRange,
        build: impl BuildNaturalRange,
    ) -> Option<TypeConstraint> {
        todo!()
    }
}

struct Intersection;

impl Merge for Intersection {
    fn merge_natural_ranges(
        lhs: AnyNaturalRange,
        rhs: AnyNaturalRange,
        build: impl BuildNaturalRange,
    ) -> Option<TypeConstraint> {
        todo!()
    }
}

struct Difference;

impl Merge for Difference {
    fn merge_natural_ranges(
        lhs: AnyNaturalRange,
        rhs: AnyNaturalRange,
        build: impl BuildNaturalRange,
    ) -> Option<TypeConstraint> {
        todo!()
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum TypeConstraintKind {
    NegInteger,
    PosInteger,

    NegRational,
    PosRational,

    NegFiniteF32,
    PosFiniteF32,

    NegFiniteF64,
    PosFiniteF64,

    UnitValue,

    AsciiChar,
    UnicodeChar,

    UnitStringChar,
    StringLen,
    StringChar,

    UnitListItemType,
    ListLen,
    ListItemType,

    UnitSetItemType,
    SetLen,
    SetItemType,

    UnitMapItemType,
    MapLen,
    MapItemType,

    StructDefinition,

    FunctionSignature,

    Distinct,

    TraitBound,

    Meta,
}

impl TypeConstraintKind {
    /// Which flag this constraint corresponds to.
    fn flag(self) -> TypeFlag {
        match self {
            Self::NegInteger => TypeFlag::NegInteger,
            Self::PosInteger => TypeFlag::PosInteger,
            Self::NegRational => TypeFlag::NegRational,
            Self::PosRational => TypeFlag::PosRational,
            Self::NegFiniteF32 => TypeFlag::NegFiniteF32,
            Self::PosFiniteF32 => TypeFlag::PosFiniteF32,
            Self::NegFiniteF64 => TypeFlag::NegFiniteF64,
            Self::PosFiniteF64 => TypeFlag::PosFiniteF64,
            Self::UnitValue => TypeFlag::UnitValue,
            Self::AsciiChar => TypeFlag::AsciiChar,
            Self::UnicodeChar => TypeFlag::UnicodeChar,
            Self::UnitStringChar => TypeFlag::UnitString,
            Self::StringLen | Self::StringChar => TypeFlag::String,
            Self::UnitListItemType => TypeFlag::UnitList,
            Self::ListLen | Self::ListItemType => TypeFlag::List,
            Self::UnitSetItemType => TypeFlag::UnitSet,
            Self::SetLen | Self::SetItemType => TypeFlag::Set,
            Self::UnitMapItemType => TypeFlag::UnitMap,
            Self::MapLen | Self::MapItemType => TypeFlag::Map,
            Self::StructDefinition => TypeFlag::Struct,
            Self::FunctionSignature => TypeFlag::Function,
            Self::Distinct => TypeFlag::Distinct,
            Self::TraitBound => TypeFlag::TraitBound,
            Self::Meta => TypeFlag::Meta,
        }
    }
}

/// A series of multiple non-overlapping ranges.
///
/// When possible, ranges must be merged. Ranges will always be in their most greedy form. I.e.
/// `1, 2, 3, 5, 7` must be represented as `1, 2, 3` followed by `5, 7`.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Ranges<T>(Vec<T>);

enum AnyNaturalRange {
    SmallRange(SmallRange),
    NaturalRange(Arc<NaturalRange>),
    NaturalRanges(Arc<Ranges<NaturalRange>>),
}

// TODO: derive From with some enum crate, forgot which one

impl From<SmallRange> for AnyNaturalRange {
    fn from(value: SmallRange) -> Self {
        todo!()
    }
}

impl From<Arc<NaturalRange>> for AnyNaturalRange {
    fn from(value: Arc<NaturalRange>) -> Self {
        todo!()
    }
}

impl From<Arc<Ranges<NaturalRange>>> for AnyNaturalRange {
    fn from(value: Arc<Ranges<NaturalRange>>) -> Self {
        todo!()
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
