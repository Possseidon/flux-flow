use std::{mem::take, sync::Arc};

use super::{Value, ValueStorage};

impl From<()> for Value {
    fn from(_: ()) -> Self {
        Self(ValueStorage::Unit)
    }
}

impl TryFrom<Value> for () {
    type Error = Value;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value.0 {
            ValueStorage::Unit => Ok(()),
            _ => Err(value),
        }
    }
}

impl<'a> TryFrom<&'a Value> for () {
    type Error = &'a Value;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        match value.0 {
            ValueStorage::Unit => Ok(()),
            _ => Err(value),
        }
    }
}

macro_rules! impl_from_small_tuple_for_value {
    ( $TupleN:ident $( $T:ident $t:ident )* ) => {
        impl< $( $T: Into<Value>, )* > From<( $( $T, )* )> for Value {
            fn from(( $( $t, )* ): ( $( $T, )* )) -> Self {
                Self(ValueStorage::$TupleN(Arc::new([ $( $t.into(), )* ])))
            }
        }

        // impl<'a, $( $T, )* > From<&'a ( $( $T, )* )> for Value
        // where
        //     $( &'a $T: Into<Value>, )*
        // {
        //     fn from(( $( $t, )* ): &'a ( $( $T, )* )) -> Self {
        //         Self(ValueStorage::$TupleN(Arc::new(( $( $t.into() ),* ))))
        //     }
        // }

        impl< $( $T: TryFrom<Value, Error = Value>, )* > TryFrom<Value>
            for ( $( $T, )* )
        where
            $( Value: From<$T>, )*
        {
            type Error = Value;

            fn try_from(value: Value) -> Result<Self, Self::Error> {
                match value.0 {
                    ValueStorage::$TupleN(mut values) => {
                        let values_mut = Arc::make_mut(&mut values);
                        #[allow(unused_parens)]
                        let [ $( $t, )* ] = take(values_mut);
                        match ( $( $t.try_into(), )* ) {
                            ( $( Ok($t), )* ) => Ok(( $( $t, )* )),
                            ( $( $t, )* ) => {
                                *values_mut = [ $( $t.map(Value::from).unwrap_or_else(|value| value), )* ];
                                Err(Value(ValueStorage::$TupleN(values)))
                            }
                        }
                    }
                    _ => Err(value),
                }
            }
        }

        impl<'a, $( $T: TryFrom<&'a Value, Error = &'a Value>, )* >
            TryFrom<&'a Value> for ( $( $T, )* )
        {
            type Error = &'a Value;

            fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
                if let Value(ValueStorage::$TupleN(box_value)) = value {
                    #[allow(unused_parens)]
                    let [ $( $t, )* ] = box_value.as_ref();
                    if let ( $( Ok($t), )* ) = ( $( $t.try_into(), )* ) {
                        return Ok(( $( $t, )* ));
                    }
                }
                Err(value)
            }
        }
    };
}

impl_from_small_tuple_for_value!(Tuple1 T0 t0);
impl_from_small_tuple_for_value!(Tuple2 T0 t0 T1 t1);
impl_from_small_tuple_for_value!(Tuple3 T0 t0 T1 t1 T2 t2);

macro_rules! unroll {
    ( $macro:ident => $( $T:ident $t:ident )* | ) => {};
    (
        $macro:ident => $( $T:ident $t:ident )* |
        $count_first_unroll:literal $T_first_unroll:ident $t_first_unroll:ident $( $count_unroll:literal $T_unroll:ident $t_unroll:ident )*
    ) => {
        unroll!($macro => $( $T $t )* | $( $count_unroll $T_unroll $t_unroll )* );
        $macro!( $count_first_unroll $( $T $t )* $T_first_unroll $t_first_unroll $( $T_unroll $t_unroll )* );
    };
}

macro_rules! impl_from_big_tuple_for_value {
    ( $count:literal $( $T:ident $t:ident )* ) => {
        impl< $( $T: Into<Value>, )* > From<( $( $T, )* )> for Value {
            fn from(( $( $t, )* ): ( $( $T, )* )) -> Self {
                Self(ValueStorage::Tuple4OrMore(Arc::new(vec![ $( $t.into(), )* ].into_boxed_slice())))
            }
        }

        impl< $( $T: TryFrom<Value, Error = Value>, )* > TryFrom<Value> for ( $( $T, )* )
        where
            $( Value: From<$T>, )*
        {
            type Error = Value;
            fn try_from(value: Value) -> Result<Self, Self::Error> {
                match value.0 {
                    ValueStorage::Tuple4OrMore(mut values) if values.len() == $count => {
                        let values_mut = Arc::make_mut(&mut values);
                        match {
                            let mut values_iter = values_mut.iter_mut().map(|value| take(value));
                            $( let $t = values_iter.next().unwrap().try_into(); )*
                            ( $( $t, )* )
                        } {
                            ( $( Ok($t), )* ) => Ok(( $( $t, )* )),
                            ( $( $t, )* ) => {
                                let mut values_iter = values_mut.iter_mut();
                                $( *values_iter.next().unwrap() = $t.map(Value::from).unwrap_or_else(|value| value); )*
                                Err(Value(ValueStorage::Tuple4OrMore(values)))
                            }
                        }
                    }
                    _ => Err(value),
                }
            }
        }

        impl<'a, $( $T: TryFrom<&'a Value>, )* > TryFrom<&'a Value> for ( $( $T, )* ) {
            type Error = &'a Value;

            fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
                if let Value(ValueStorage::Tuple4OrMore(values)) = value {
                    if let [ $( $t, )* ] = &values[..] {
                        if let ( $( Ok($t), )* ) = ( $( $t.try_into(), )* ) {
                            return Ok(( $( $t, )* ));
                        }
                    }
                }
                Err(value)
            }
        }
    };
}

unroll!(impl_from_big_tuple_for_value =>
    T0 t0 T1 t1 T2 t2 |
    12 T11 t11 11 T10 t10 10 T9 t9 9 T8 t8 8 T7 t7 7 T6 t6 6 T5 t5 5 T4 t4 4 T3 t3
);

pub struct Tuple(pub Box<[Value]>);

impl From<Tuple> for Value {
    fn from(mut value: Tuple) -> Self {
        match value.0.len() {
            0 => Self(ValueStorage::Unit),
            1 => Self(ValueStorage::Tuple1(Arc::new([value
                .0
                .iter_mut()
                .map(take)
                .next()
                .unwrap()]))),
            2 => {
                let mut values_iter = value.0.iter_mut().map(take);
                Self(ValueStorage::Tuple2(Arc::new([
                    values_iter.next().unwrap(),
                    values_iter.next().unwrap(),
                ])))
            }
            3 => {
                let mut values_iter = value.0.iter_mut().map(take);
                Self(ValueStorage::Tuple3(Arc::new([
                    values_iter.next().unwrap(),
                    values_iter.next().unwrap(),
                    values_iter.next().unwrap(),
                ])))
            }
            _ => Self(ValueStorage::Tuple4OrMore(Arc::new(value.0))),
        }
    }
}

impl From<&Tuple> for Value {
    fn from(value: &Tuple) -> Self {
        match &value.0[..] {
            [] => Self(ValueStorage::Unit),
            [t0] => Self(ValueStorage::Tuple1(Arc::new([t0.clone()]))),
            [t0, t1] => Self(ValueStorage::Tuple2(Arc::new([t0.clone(), t1.clone()]))),
            [t0, t1, t2] => Self(ValueStorage::Tuple3(Arc::new([
                t0.clone(),
                t1.clone(),
                t2.clone(),
            ]))),
            values => Self(ValueStorage::Tuple4OrMore(Arc::new(
                values.iter().cloned().collect(),
            ))),
        }
    }
}
