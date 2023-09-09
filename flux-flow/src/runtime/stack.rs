mod primitives;

use std::{any::Any, mem::size_of, sync::Arc};

use num::BigInt;

use crate::compiler::typing::Type;

// TODO: stack access is very easy to get wrong
//       try to keep it private and only possible through macro magic that prevents type mismatches

/// A stack of values represented as [`Data`] entries internally.
#[derive(Clone, Debug, Default)]
pub struct Stack {
    data: Vec<Data>,
}

impl Stack {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push<const DATA_LEN: usize>(&mut self, value: impl StackValue<DATA_LEN>) -> usize {
        let index = self.data.len();
        self.data.resize(index + DATA_LEN, Default::default());
        self.put(index, value);
        index
    }

    pub fn pop<T: StackValue<DATA_LEN>, const DATA_LEN: usize>(&mut self) -> T {
        let index = self.data.len() - DATA_LEN;
        let value = self.take(index);
        self.data.truncate(index);
        value
    }

    pub fn put<const DATA_LEN: usize>(&mut self, index: usize, value: impl StackValue<DATA_LEN>) {
        value.put_on_stack(
            (&mut self.data[index..index + DATA_LEN])
                .try_into()
                .expect("slice should have correct size"),
        );
    }

    pub fn take<T: StackValue<DATA_LEN>, const DATA_LEN: usize>(&mut self, index: usize) -> T {
        let value = T::take_from_stack(
            (&mut self.data[index..index + DATA_LEN])
                .try_into()
                .expect("slice should have correct size"),
        );
        self.data.truncate(index);
        value
    }

    pub fn get<T: StackValueRef<DATA_LEN>, const DATA_LEN: usize>(&self, index: usize) -> &T {
        T::stack_ref(
            self.data[index..index + DATA_LEN]
                .try_into()
                .expect("slice should have correct size"),
        )
    }

    pub fn get_mut<T: StackValueRef<DATA_LEN>, const DATA_LEN: usize>(
        &mut self,
        index: usize,
    ) -> &mut T {
        T::stack_mut(
            (&mut self.data[index..index + DATA_LEN])
                .try_into()
                .expect("slice should have correct size"),
        )
    }
}

/// A single value on the stack.
///
/// A single entry is 32 bytes in size. This might seem big, but keep in mind that a single entry
/// can represent multiple values.
///
/// Out of these 32 bytes, a single byte is used as a discriminant to distinguish between various
/// managed values. All unmanaged values can be represented by the [`Data::Bits`] variant or the
/// bits fields of other variants.
#[derive(Clone, Debug)]
#[repr(u8)]
pub enum Data {
    /// 31 bytes of data that can be used to represent anything that isn't managed.
    ///
    /// While similar to a single `[u8; 31]`, separating the different fields enforces alignment.
    ///
    /// Other variants also have come with such bytes that can be used freely.
    Bits {
        u8: u8,
        u16: u16,
        u32: u32,
        u64: u64,
        u128: u128,
    },
    /// Wraps a single other data entry, mainly giving an additional 23 bytes of raw bits.
    ///
    /// `data` is wrapped in an [`Option`] just to give one more additional possible value.
    DataBox {
        u8: u8,
        u16: u16,
        u32: u32,
        data: Option<Arc<Data>>,
        u128: u128,
    },
    /// Wraps a slice of multiple data entries, used for storing larger structs inside e.g. a list.
    DataSlice {
        u8: u8,
        u16: u16,
        u32: u32,
        u64: u64,
        data: Option<Arc<[Data]>>,
    },
    /// A big integer stored on the heap.
    BigInt {
        u8: u8,
        u16: u16,
        u32: u32,
        big_int: Option<Arc<BigInt>>,
        u128: u128,
    },
    /// Wraps an [`Any`] value.
    Any {
        u8: u8,
        u16: u16,
        u32: u32,
        u64: u64,
        any: Option<Arc<dyn Any>>,
    },
    // TODO: Optimized versions that combine multiple managed types together.
}

const _: () = assert!(size_of::<Data>() == 32);

impl Data {
    pub fn new_u8(u8: u8) -> Self {
        Data::Bits {
            u8,
            u16: 0,
            u32: 0,
            u64: 0,
            u128: 0,
        }
    }

    pub fn new_u16(u16: u16) -> Self {
        Data::Bits {
            u8: 0,
            u16,
            u32: 0,
            u64: 0,
            u128: 0,
        }
    }

    pub fn new_u32(u32: u32) -> Self {
        Data::Bits {
            u8: 0,
            u16: 0,
            u32,
            u64: 0,
            u128: 0,
        }
    }

    pub fn new_u64(u64: u64) -> Self {
        Data::Bits {
            u8: 0,
            u16: 0,
            u32: 0,
            u64,
            u128: 0,
        }
    }

    pub fn new_u128(u128: u128) -> Self {
        Data::Bits {
            u8: 0,
            u16: 0,
            u32: 0,
            u64: 0,
            u128,
        }
    }
}

impl Default for Data {
    fn default() -> Self {
        Self::Bits {
            u8: 0,
            u16: 0,
            u32: 0,
            u64: 0,
            u128: 0,
        }
    }
}

/// A value that can be put on and taken from a [`Stack`] in the form of a slice of [`Data`].
pub trait StackValue<const DATA_LEN: usize> {
    // fn runtime_type() -> RuntimeType;

    /// Puts [`self`] on the the [`Stack`], overwriting any previous values.
    ///
    /// This function can assume the given slice to have at least [`StackIO::STACK_SIZE`] of space.
    fn put_on_stack(self, stack: &mut [Data; DATA_LEN]);
    /// Take a value from the [`Stack`], replacing it with [`Data::Bits`] if it needs to be moved.
    ///
    /// This function can assume the given slice to have at least [`StackIO::STACK_SIZE`] of space.
    fn take_from_stack(stack: &mut [Data; DATA_LEN]) -> Self;
}

/// Implemented on top of [`StackValue`] for types that are stored without any conversion and can
/// thus be referenced directly.
///
/// The main benefit this brings is avoding unnecessary cloning of [`Arc`].
pub trait StackValueRef<const DATA_LEN: usize>: StackValue<DATA_LEN> {
    /// Returns a reference to the underlying value on the stack.
    fn stack_ref(stack: &[Data; DATA_LEN]) -> &Self;
    /// Returns a mutable reference to the underlyign value on the stack.
    fn stack_mut(stack: &mut [Data; DATA_LEN]) -> &mut Self;
}

// TODO: Move all of the below

/// A function which, when called, modifies a [`Stack`].
///
/// The "call" must uphold a contract in regards to argument type, return type as well
/// predictability in regards to being called multiple times with the same argument.
trait Function {
    /// The argument type of the function.
    fn arg_type(&self) -> Type;

    /// The general return type of the function.
    ///
    /// If the argument type is known to be a subset of the actual argument type,
    /// [`Function::ret_type_for`] can be used to get a more specific return type.
    ///
    /// For example, if the range of two arbitrary integers is known, the return type of a function
    /// that calculates their sum can be inferred to be the minimum and maximum sum that could be
    /// produced.
    ///
    /// This is can even be useful for impure functions. A UUID generation function for example
    /// could take the UUID version as an argument and guarantee that the returned UUID matches.
    fn ret_type(&self) -> Type {
        self.ret_type_for(&self.arg_type())
    }

    /// The return type of the function, potentially specialized on the argument type.
    ///
    /// The `arg_type` must be a subset of the argument type of the contract.
    fn ret_type_for(&self, arg_type: &Type) -> Type;

    /// How the function behaves when called with the same argument.
    ///
    /// [`None`] means, the function does not have any guarantees and is completely unpredictable.
    fn predictability(&self) -> Option<FunctionPredictability>;

    /// Calls the function using the provided [`Stack`], while following its contract.
    fn call(&self, stack: &mut Stack);
}

/// What guarantees a function has when called with the same arguments.
enum FunctionPredictability {
    /// Given the same argument, the function will always yield the same return value.
    ///
    /// Pure functions do not modify any external state or rely on any external input other than
    /// its argument. This should generally be the "golden default" for functions, as it promotes
    /// code that is easier to reason about and test.
    Pure,
    /// Given the same argument, this function will produce ever increasing output values.
    ///
    /// This is mainly used for returning the current time while allowing the language to reason
    /// about the chronological order of different calls. It can also be used for functions that
    /// e.g. generate an ever increasing sequence of numbers.
    Monotonic {
        /// Allows changing the direction of the monotonicity between increasing and decreasing.
        direction: MonotonicDirection,
        /// Whether consecutive calls cannot return the exact same value.
        strict: bool,
    },
    /// Given the same argument, the function will **never** return the same output.
    ///
    /// This is useful for e.g. UUID generation.
    Unique,
}

enum MonotonicDirection {
    Increasing,
    Decreasing,
}
