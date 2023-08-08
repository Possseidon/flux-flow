use crate::typing::Type;

use super::{StackIO, StackReader, StackSize, StackWriter};

impl StackIO for bool {
    fn size(ty: &Type) -> StackSize {
        StackSize {
            bit_len: 1,
            ..Default::default()
        }
    }

    fn write(self, ty: &Type, mut stack: StackWriter) {
        stack.write_bit(self);
    }

    fn read<'a>(ty: &Type, mut stack: StackReader<'a>) -> Self
    where
        Self: 'a,
    {
        stack.read_bit()
    }
}
