use std::{
    mem::take,
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Sub, SubAssign},
    sync::Arc,
};

use enumset::{EnumSet, EnumSetType};
use paste::paste;

#[derive(Clone, Debug, Default)]
pub(crate) enum UnionType {
    #[default]
    Never,
    One(UnionTypeOne),
    Many(Arc<UnionTypeMany>),
}

macro_rules! impl_union_type_binop {
    ( $Op:ident $f:ident $op:tt ) => {
        impl $Op for &UnionType {
            type Output = UnionType;

            fn $f(self, rhs: Self) -> Self::Output {
                (self.into_many() $op rhs.into_many()).normalize()
            }
        }
    };
}

impl_union_type_binop!(BitOr bitor |);
impl_union_type_binop!(BitAnd bitand &);
impl_union_type_binop!(BitXor bitxor ^);
impl_union_type_binop!(Sub sub -);

macro_rules! impl_union_type_many_binop {
    ( $Op:ident $f:ident $op:tt $( $field:ident )* ) => {

        impl $Op<UnionTypeMany> for UnionTypeMany {
            type Output = UnionTypeMany;

            fn $f(self, rhs: UnionTypeMany) -> Self::Output {
                UnionTypeMany {
                    $( $field: self.$field $op rhs.$field, )*
                }
            }
        }
    };
}

macro_rules! impl_union_type {
    ( $( $field:ident: $ty:ty, )* ) => { paste! {
        #[derive(Debug, EnumSetType)]
        enum UnionTypeKind {
            $( [< $field:camel >], )*
        }

        #[derive(Clone, Debug)]
        pub(crate) enum UnionTypeOne {
            $( [< $field:camel >]($ty), )*
        }

        #[derive(Clone, Debug, Default)]
        pub(crate) struct UnionTypeMany {
            $( $field: $ty, )*
        }

        impl UnionType {
            fn into_many(self) -> UnionTypeMany {
                match self {
                    UnionType::Never => UnionTypeMany::default(),
                    UnionType::One(one) => match one {
                        $( UnionTypeOne::[< $field:camel >]($field) => UnionTypeMany {
                            $field,
                            ..Default::default()
                        }, )*
                    },
                    UnionType::Many(many) => Arc::try_unwrap(many).unwrap_or_else(|arc| (*arc).clone()),
                }
            }
        }

        impl UnionTypeOne {
            fn variant_kind(&self) -> UnionTypeKind {
                match self {
                    $( Self::[< $field:camel >](_) => UnionTypeKind::[< $field:camel >], )*
                }
            }
        }

        impl UnionTypeMany {
            fn variant_kinds(&self) -> EnumSet<UnionTypeKind> {
                let mut kinds = EnumSet::new();
                $( if !self.$field.is_never() { kinds |= UnionTypeKind::[< $field:camel >]; } )*
                kinds
            }

            fn normalize(self) -> UnionType {
                let kinds = self.variant_kinds();
                if kinds == EnumSet::new() {
                    UnionType::Never
                } else $( if kinds == UnionTypeKind::[< $field:camel >] {
                    UnionType::One(UnionTypeOne::[< $field:camel >](self.$field))
                } else )* {
                    UnionType::Many(Arc::new(self))
                }
            }
        }

        impl_union_type_many_binop!(BitOr bitor | $( $field )* );
        impl_union_type_many_binop!(BitAnd bitand & $( $field )* );
        impl_union_type_many_binop!(BitXor bitxor ^ $( $field )* );
        impl_union_type_many_binop!(Sub sub - $( $field )* );
    } };
}

impl_union_type! {
    boolean: super::boolean::BooleanType,
    unit: super::number::unit::UnitType,
    // rational: super::number::rational::RationalType,
    // float: super::number::float::FloatType,
    // char: super::string::CharType,
    // string: super::string::StringType,
    // uuid: super::uuid::UuidType,
    // instant: super::instant::InstantType,
    // list: super::collections::list::ListType,
    // set: super::collections::set::SetType,
    // map: super::collections::map::MapType,
    // struct_types: super::struct_type::StructTypes,
    // function_types: super::function::FunctionTypes,
    // meta_types: super::meta::MetaTypes,
    // trait_types: super::trait_type::TraitTypes,
    // wrapped_types: super::wrapper::WrappedTypes,
}
