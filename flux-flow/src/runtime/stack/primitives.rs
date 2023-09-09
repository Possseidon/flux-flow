use paste::paste;

use super::{Data, StackValue, StackValueRef};

impl StackValue<0> for () {
    fn put_on_stack(self, _stack: &mut [Data; 0]) {}
    fn take_from_stack(_stack: &mut [Data; 0]) -> Self {}
}

macro_rules! impl_stack_value_for_u {
    ( $( $N:literal )* ) => { paste! { $(
        impl StackValue<1> for [<u $N>] {
            fn put_on_stack(self, stack: &mut [Data; 1]) {
                stack[0] = Data::[<new_u $N>](self);
            }

            fn take_from_stack(stack: &mut [Data; 1]) -> Self {
                match stack[0] {
                    Data::Bits { [<u $N>], .. } => [<u $N>],
                    _ => panic!(concat!(
                        "u",
                        stringify!($N),
                        " should be represented by Data::Bits",
                    )),
                }
            }
        }

        impl StackValueRef<1> for [<u $N>] {
            fn stack_ref(stack: &[Data; 1]) -> &Self {
                match &stack[0] {
                    Data::Bits { [<u $N>], .. } => [<u $N>],
                    _ => panic!(concat!(
                        "u",
                        stringify!($N),
                        " should be represented by Data::Bits",
                    )),
                }
            }

            fn stack_mut(stack: &mut [Data; 1]) -> &mut Self {
                match &mut stack[0] {
                    Data::Bits { [<u $N>], .. } => [<u $N>],
                    _ => panic!(concat!(
                        "u",
                        stringify!($N),
                        " should be represented by Data::Bits",
                    )),
                }
            }
        }
    )* } };
}

impl_stack_value_for_u!(8 16 32 64 128);

macro_rules! impl_stack_value_for_i {
    ( $( $N:literal )* ) => { paste! { $(
        impl StackValue<1> for [<i $N>] {
            fn put_on_stack(self, stack: &mut [Data; 1]) {
                stack[0] = Data::[<new_u $N>](self as [<u $N>]);
            }

            fn take_from_stack(stack: &mut [Data; 1]) -> Self {
                match stack[0] {
                    Data::Bits { [<u $N>], .. } => [<u $N>] as [<i $N>],
                    _ => panic!(concat!(
                        "u",
                        stringify!($N),
                        " should be represented by Data::Bits",
                    )),
                }
            }
        }
    )* } };
}

impl_stack_value_for_i!(8 16 32 64 128);

impl StackValue<1> for bool {
    fn put_on_stack(self, stack: &mut [Data; 1]) {
        stack[0] = Data::new_u8(self as u8);
    }

    fn take_from_stack(stack: &mut [Data; 1]) -> Self {
        match stack[0] {
            Data::Bits { u8, .. } => u8 != 0,
            _ => panic!("bool should be represented by Data::Bits"),
        }
    }
}

macro_rules! impl_stack_value_for_f {
    ( $( $N:literal )* ) => { paste!{ $(
        impl StackValue<1> for [<f $N>] {
            fn put_on_stack(self, stack: &mut [Data; 1]) {
                stack[0] = Data::[<new_u $N>](self.to_bits());
            }

            fn take_from_stack(stack: &mut [Data; 1]) -> Self {
                match stack[0] {
                    Data::Bits { [<u $N>], .. } => Self::from_bits([<u $N>]),
                    _ => panic!(concat!(
                        "f",
                        stringify!($N),
                        " should be represented by Data::Bits",
                    )),
                }
            }
        }
    )* } };
}

impl_stack_value_for_f!(32 64);
