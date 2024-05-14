pub mod compiler;
pub mod static_type;

// type BitStorage = Vec<usize>;

// type AnyValue = Arc<dyn Any + Send + Sync>;
// type AnyStorage = Vec<Option<AnyValue>>;

// /// A stack value that can be represented by a fixed number of bits.
// trait BitStackValue {
//     const LEN: usize;

//     fn static_type() -> StaticType;

//     fn push(self, bits: &mut impl Extend<usize>);
// }

// struct Stack {
//     bits: BitStorage,
//     anys: AnyStorage,
// }

// impl Stack {
//     fn push_bit(&mut self, value: impl BitStackValue) {
//         value.push(&mut self.bits)
//     }

//     fn push_any(&mut self, value: AnyValue) {
//         self.anys.push(Some(value));
//     }
// }

// struct StackIndex {
//     bit: BitIndex,
//     any: AnyIndex,
// }

// struct BitIndex(usize);

// struct AnyIndex(usize);

// impl BitStackValue for True {
//     const LEN: usize = 0;

//     fn static_type() -> StaticType {
//         todo!()
//     }

//     fn push(self, _bits: &mut impl Extend<usize>) {}
// }

// impl BitStackValue for False {
//     const LEN: usize = 0;

//     fn static_type() -> StaticType {
//         todo!()
//     }

//     fn push(self, _bits: &mut impl Extend<usize>) {}
// }

// impl BitStackValue for Maybe {
//     const LEN: usize = 0;

//     fn static_type() -> StaticType {
//         todo!()
//     }

//     fn push(self, _bits: &mut impl Extend<usize>) {}
// }

// impl BitStackValue for bool {
//     const LEN: usize = 1;

//     fn static_type() -> StaticType {
//         todo!()
//     }

//     fn push(self, bits: &mut impl Extend<usize>) {
//         bits.extend(Some(self.into()));
//     }
// }
