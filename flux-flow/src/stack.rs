mod boolean;

use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    iter::repeat,
    mem::size_of,
    sync::Arc,
};

use bitvec::vec::BitVec;
use num::BigInt;

use crate::{
    typing::Type,
    value::{FatValue, Value},
};

// TODO: Move
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Function(fn(&mut Stack));

/// The stack used by the interpreter.
///
/// It can store some values very efficiently by utilizing separate specialized vecs.
///
/// All vecs can contain padding in the form of `false`, `0` or `None`.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Stack {
    /// Can store types that have two possible values.
    bit_slots: BitVec,
    /// Can stores types that have up to [`u8`] many possible values.
    u8_slots: Vec<u8>,
    /// Can stores types that have up to [`u16`] many possible values.
    u16_slots: Vec<u16>,
    /// Can stores types that have up to [`u32`] many possible values.
    u32_slots: Vec<u32>,
    /// Can stores types that have up to [`u64`] many possible values.
    u64_slots: Vec<u64>,
    /// Stores 16 byte wide types that require memory management.
    managed_slots: Vec<Option<ManagedValue>>,
    /// Stores 24 byte wide types that require memory management.
    fat_managed_slots: Vec<Option<FatManagedValue>>,
}

/// Combined [`usize`] types for the different vecs in [`Stack`] for indexing and length.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct StackSize {
    bit_len: usize,
    u8_len: usize,
    u16_len: usize,
    u32_len: usize,
    u64_len: usize,
    managed_len: usize,
    fat_managed_len: usize,
}

/// Used by the [`StackIO`] trait to write values to the stack.
pub struct StackWriter<'a> {
    stack: &'a mut Stack,
    offset: StackSize,
}

impl<'a> StackWriter<'a> {
    pub fn write_bit(&mut self, value: bool) {
        self.stack.bit_slots.set(self.offset.bit_len, value);
        self.offset.bit_len += 1;
    }

    pub fn write_u8(&mut self, value: u8) {
        self.stack.u8_slots[self.offset.u8_len] = value;
        self.offset.u8_len += 1;
    }

    pub fn write_u16(&mut self, value: u16) {
        self.stack.u16_slots[self.offset.u16_len] = value;
        self.offset.u16_len += 1;
    }

    pub fn write_u32(&mut self, value: u32) {
        self.stack.u32_slots[self.offset.u32_len] = value;
        self.offset.u32_len += 1;
    }

    pub fn write_u64(&mut self, value: u64) {
        self.stack.u64_slots[self.offset.u64_len] = value;
        self.offset.u64_len += 1;
    }

    pub fn write_managed(&mut self, value: Option<ManagedValue>) {
        self.stack.managed_slots[self.offset.managed_len] = value;
        self.offset.managed_len += 1;
    }

    pub fn write_fat_managed(&mut self, value: Option<FatManagedValue>) {
        self.stack.fat_managed_slots[self.offset.fat_managed_len] = value;
        self.offset.fat_managed_len += 1;
    }
}

/// Used by the [`StackIO`] trait to read values from the stack.
pub struct StackReader<'a> {
    stack: &'a Stack,
    offset: StackSize,
}

impl<'a> StackReader<'a> {
    pub fn read_bit(&mut self) -> bool {
        let value = self.stack.bit_slots[self.offset.bit_len];
        self.offset.bit_len += 1;
        value
    }

    pub fn read_u8(&mut self) -> u8 {
        let value = self.stack.u8_slots[self.offset.u8_len];
        self.offset.u8_len += 1;
        value
    }

    pub fn read_u16(&mut self) -> u16 {
        let value = self.stack.u16_slots[self.offset.u16_len];
        self.offset.u16_len += 1;
        value
    }

    pub fn read_u32(&mut self) -> u32 {
        let value = self.stack.u32_slots[self.offset.u32_len];
        self.offset.u32_len += 1;
        value
    }

    pub fn read_u64(&mut self) -> u64 {
        let value = self.stack.u64_slots[self.offset.u64_len];
        self.offset.u64_len += 1;
        value
    }

    pub fn read_managed(&mut self) -> &Option<ManagedValue> {
        let value = &self.stack.managed_slots[self.offset.managed_len];
        self.offset.managed_len += 1;
        value
    }

    pub fn read_fat_managed(&mut self) -> &Option<FatManagedValue> {
        let value = &self.stack.fat_managed_slots[self.offset.fat_managed_len];
        self.offset.fat_managed_len += 1;
        value
    }
}

/// A single managed value on the stack.
///
/// TODO: Store collections of some types more efficiently.
///       E.g. vecs holding primitives or vec-of-struct as struct-of-vec internally.
///       Although the latter might compilcate some things (e.g. sorting).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ManagedValue {
    BigInt(Arc<BigInt>),
    String(Arc<String>),
    Vec(Arc<Vec<Value>>),
    FatVec(Arc<Vec<FatValue>>),
    Deque(Arc<VecDeque<Value>>),
    FatDeque(Arc<VecDeque<FatValue>>),
    Set(Arc<BTreeSet<Value>>),
    Map(Arc<BTreeMap<Value, Value>>),
    FatKeyMap(Arc<BTreeMap<FatValue, Value>>),
    FatValueMap(Arc<BTreeMap<Value, FatValue>>),
    FatMap(Arc<BTreeMap<FatValue, FatValue>>),
    Function(Function),
}

const _: () = assert!(size_of::<ManagedValue>() == 16);
const _: () = assert!(size_of::<Option<ManagedValue>>() == 16);

/// A single managed value on the stack that stores a fat pointer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FatManagedValue {
    CompactString(Arc<str>),
    CompactList(Arc<[Value]>),
    Type(Type),
}

const _: () = assert!(size_of::<FatManagedValue>() == 24);
const _: () = assert!(size_of::<Option<FatManagedValue>>() == 24);

impl Stack {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn size(&self) -> StackSize {
        StackSize {
            bit_len: self.bit_slots.len(),
            u8_len: self.u8_slots.len(),
            u16_len: self.u16_slots.len(),
            u32_len: self.u32_slots.len(),
            u64_len: self.u64_slots.len(),
            managed_len: self.managed_slots.len(),
            fat_managed_len: self.fat_managed_slots.len(),
        }
    }

    pub fn write<T: StackIO>(&mut self, ty: &Type, value: T, offset: StackSize) {
        value.write(
            ty,
            StackWriter {
                stack: self,
                offset,
            },
        );
    }

    pub fn push<T: StackIO>(&mut self, ty: &Type, value: T) {
        let offset = self.size();
        let size = T::size(ty);
        self.bit_slots
            .resize(self.bit_slots.len() + size.bit_len, false);
        self.u8_slots.resize(self.u8_slots.len() + size.u8_len, 0);
        self.u16_slots
            .resize(self.u16_slots.len() + size.u16_len, 0);
        self.u32_slots
            .resize(self.u32_slots.len() + size.u32_len, 0);
        self.u64_slots
            .resize(self.u64_slots.len() + size.u64_len, 0);
        self.managed_slots
            .resize(self.managed_slots.len() + size.managed_len, None);
        self.fat_managed_slots
            .resize(self.fat_managed_slots.len() + size.fat_managed_len, None);

        self.write(ty, value, offset);
    }

    pub fn read<'a, T: StackIO + 'a>(&'a self, ty: &Type, offset: StackSize) -> T {
        T::read(
            ty,
            StackReader {
                stack: self,
                offset,
            },
        )
    }

    pub fn pop<T: StackIO>(&mut self, ty: &Type) -> T {
        let old_size = self.size();
        let value_size = T::size(ty);
        let offset = StackSize {
            bit_len: old_size.bit_len - value_size.bit_len,
            u8_len: old_size.u8_len - value_size.u8_len,
            u16_len: old_size.u16_len - value_size.u16_len,
            u32_len: old_size.u32_len - value_size.u32_len,
            u64_len: old_size.u64_len - value_size.u64_len,
            managed_len: old_size.managed_len - value_size.managed_len,
            fat_managed_len: old_size.fat_managed_len - value_size.fat_managed_len,
        };

        let value = self.read(ty, offset);

        self.bit_slots.truncate(offset.bit_len);
        self.u8_slots.truncate(offset.u8_len);
        self.u16_slots.truncate(offset.u16_len);
        self.u32_slots.truncate(offset.u32_len);
        self.u64_slots.truncate(offset.u64_len);
        self.managed_slots.truncate(offset.managed_len);
        self.fat_managed_slots.truncate(offset.fat_managed_len);

        value
    }
}

/// How to read and write values from and to the [`Stack`].
///
/// It is implemented by most of Rust's builtin types, as well as for [`Value`], [`FatValue`] and
/// [`ValueRef`].
///
/// This trait is intentionally not split between read and write, since it does not make sense to only
/// implement one. It does not make sense to write something if you never read it and you likewise
/// cannot read something if you never wrote it in the first place.
///
/// When implementing this trait, it is important to ensure that corresponding read and write
/// methods are always compatible with each other.
pub trait StackIO {
    fn size(ty: &Type) -> StackSize;

    fn write(self, ty: &Type, stack: StackWriter);

    fn read<'a>(ty: &Type, stack: StackReader<'a>) -> Self
    where
        Self: 'a;
}
