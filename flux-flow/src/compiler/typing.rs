use std::{
    mem::replace,
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Neg, Sub, SubAssign},
};

use self::union::UnionType;

pub mod boolean;
pub mod collections;
pub mod function;
pub mod instant;
pub mod meta;
pub mod number;
pub mod string;
pub mod struct_type;
pub mod trait_type;
pub mod union;
pub mod uuid;
pub mod wrapper;

// TODO: Move Value

/// Can be thought of as a [`Stack`] that only holds a single [`Data`] entry.
struct Value;

// TODO: Usually `Type` will only store a single actual type.
//       So, have an additional enum with the same variants as `Type` that it can use.

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
pub(crate) struct Type {
    union: UnionType,
    complement: bool,
}

impl Type {
    pub const NEVER: Self = Self {
        union: UnionType::Never,
        complement: false,
    };

    pub const ANY: Self = Self {
        union: UnionType::Never,
        complement: true,
    };
}

impl Neg for Type {
    type Output = Type;

    fn neg(self) -> Self::Output {
        Type {
            union: self.union,
            complement: !self.complement,
        }
    }
}

impl BitOr for Type {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self.complement, rhs.complement) {
            (false, false) => Self {
                union: self.union | rhs.union,
                complement: false,
            },
            (false, true) => Self {
                union: rhs.union - self.union,
                complement: true,
            },
            (true, false) => Self {
                union: self.union - rhs.union,
                complement: true,
            },
            (true, true) => Self {
                union: self.union & rhs.union,
                complement: true,
            },
        }
    }
}

impl BitOrAssign for Type {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = replace(self, Type::NEVER) | rhs;
    }
}

impl BitAnd for Type {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match (self.complement, rhs.complement) {
            (false, false) => Self {
                union: self.union & rhs.union,
                complement: false,
            },
            (false, true) => Self {
                union: self.union - rhs.union,
                complement: false,
            },
            (true, false) => Self {
                union: rhs.union - self.union,
                complement: false,
            },
            (true, true) => Self {
                union: self.union | rhs.union,
                complement: true,
            },
        }
    }
}

impl BitAndAssign for Type {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = replace(self, Type::NEVER) & rhs;
    }
}

impl BitXor for Type {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        if self.complement == rhs.complement {
            Self {
                union: self.union ^ rhs.union,
                complement: false,
            }
        } else {
            Self {
                union: self.union & rhs.union,
                complement: true,
            }
        }
    }
}

impl BitXorAssign for Type {
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = replace(self, Type::NEVER) ^ rhs;
    }
}

impl Sub for Type {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self.complement, rhs.complement) {
            (false, false) => Self {
                union: self.union - rhs.union,
                complement: false,
            },
            (false, true) => Self {
                union: self.union & rhs.union,
                complement: false,
            },
            (true, false) => Self {
                union: self.union | rhs.union,
                complement: true,
            },
            (true, true) => Self {
                union: rhs.union - self.union,
                complement: false,
            },
        }
    }
}

impl SubAssign for Type {
    fn sub_assign(&mut self, rhs: Self) {
        *self = replace(self, Type::NEVER) - rhs;
    }
}
