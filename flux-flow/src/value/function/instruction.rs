use crate::value::Value;

/// A list of instructions stored very compactly with separate vectors.
/// Contains an implicit [`InstructionKind::Return`] after the final real instruction.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct InstructionList {
    instructions: Vec<InstructionKind>,
    values: Vec<Value>,
    usizes: Vec<usize>,
}

impl InstructionList {
    pub(crate) fn next(&self, instruction_pointer: &mut InstructionPointer) -> InstructionRef {
        if instruction_pointer.instruction_index == self.instructions.len() {
            return InstructionRef::Return;
        }

        let mut next_instruction = || {
            let instruction = self.instructions[instruction_pointer.instruction_index];
            instruction_pointer.instruction_index += 1;
            instruction
        };

        let mut next_usize = || {
            let usize = self.usizes[instruction_pointer.usize_index];
            instruction_pointer.usize_index += 1;
            usize
        };

        let mut next_values = || {
            let count = next_usize();
            let index = instruction_pointer.value_index;
            instruction_pointer.value_index += count;
            &self.values[index..(index + count)]
        };

        let instruction = match next_instruction() {
            InstructionKind::Nop => InstructionRef::Nop,
            InstructionKind::Push => InstructionRef::Push(next_values()),
            InstructionKind::Call => InstructionRef::Call,
            InstructionKind::Return => InstructionRef::Return,
            InstructionKind::Yield => InstructionRef::Yield,
            InstructionKind::Resume => InstructionRef::Resume,
        };

        instruction
    }

    fn push(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::Nop => self.instructions.push(InstructionKind::Nop),
            Instruction::PushValue(value) => {
                self.instructions.push(InstructionKind::Push);
                self.values.push(value);
            }
            Instruction::PushValues(values) => {
                self.instructions.push(InstructionKind::Push);
                self.values.extend(values);
            }
            Instruction::Call => self.instructions.push(InstructionKind::Call),
            Instruction::Return => self.instructions.push(InstructionKind::Return),
            Instruction::Yield => self.instructions.push(InstructionKind::Yield),
            Instruction::Resume => self.instructions.push(InstructionKind::Resume),
        }
    }
}

impl FromIterator<Instruction> for InstructionList {
    fn from_iter<T: IntoIterator<Item = Instruction>>(iter: T) -> Self {
        let mut instruction_list = Self::default();
        for instruction in iter {
            instruction_list.push(instruction);
        }
        instruction_list
    }
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct InstructionPointer {
    instruction_index: usize,
    value_index: usize,
    usize_index: usize,
}

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
enum InstructionKind {
    #[default]
    Nop,
    Push,
    // PushCopy,
    // Pop,
    // PopInto,
    // CopyInto,
    // Swap,
    // Jump,
    // JumpIf,
    // JumpUnless,
    Call,
    // PopCall,
    // CallNative,
    Return,
    Yield,
    Resume,
}

pub(crate) struct StackIndexRef(pub(crate) usize);

pub(crate) struct Count(pub(crate) usize);

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum InstructionRef<'a> {
    #[default]
    Nop,
    Push(&'a [Value]),
    Call,
    Return,
    Yield,
    Resume,
}

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Instruction {
    #[default]
    Nop,
    PushValue(Value),
    PushValues(Vec<Value>),
    Call,
    Return,
    Yield,
    Resume,
}
