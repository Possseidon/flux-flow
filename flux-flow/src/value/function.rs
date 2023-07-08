pub mod instruction;
pub mod native_function;

use std::sync::Arc;

use self::instruction::{InstructionList, InstructionPointer, InstructionRef};

use super::{Value, ValueStack};

// TODO: Make sure the stack shrinks_to_fit every now and then.
//       Limit its capacity to at least twice its current size.
//       -> On every return/yield, if size() < 4 * capacity(), shrink_to()

// TODO: Instruction for relative lookup table style jumps.

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct Call {
    function: Arc<Function>,
    instruction_pointer: InstructionPointer,
}

impl Call {
    fn next_instruction(&mut self) -> InstructionRef {
        self.function
            .next_instruction(&mut self.instruction_pointer)
    }
}

/// Comparable to a `main()` function.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ExecutionEnvironment {
    value_stack: ValueStack,
    call_stack: Vec<Call>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum StepResult {
    Running,
    Done(Value),
}

impl ExecutionEnvironment {
    fn new(function: Arc<Function>, argument: Value) -> Self {
        // TODO: Check that argument matches function signature and push it into appropriate stack
        let mut value_stack = ValueStack::default();
        // value_stack.push(argument);
        Self {
            value_stack,
            call_stack: vec![Call {
                function,
                instruction_pointer: InstructionPointer::default(),
            }],
        }
    }

    fn run(&mut self) -> Value {
        loop {
            match self.step() {
                StepResult::Running => {}
                StepResult::Done(value) => break value,
            }
        }
    }

    fn current_call(&self) -> &Call {
        self.call_stack
            .last()
            .expect("call stack should not be empty")
    }

    fn current_call_mut(&mut self) -> &mut Call {
        self.call_stack
            .last_mut()
            .expect("call stack should not be empty")
    }

    fn next_instruction(&mut self) -> InstructionRef {
        self.current_call_mut().next_instruction()
    }

    fn step(&mut self) -> StepResult {
        let mut jump_to_instruction = None;

        match self.next_instruction() {
            InstructionRef::Nop => {}
            _ => todo!(),
            // Instruction::Push(value) => {
            //     self.value_stack.0.push(value);
            // }
            // Instruction::PushCopy(stack_ref) => {
            //     self.value_stack.0.push(self.stack_value(stack_ref).clone());
            // }
            // Instruction::Pop(count) => {
            //     self.value_stack
            //         .0
            //         .drain((self.value_stack.0.len() - count - 1)..);
            // }
            // Instruction::PopInto(stack_ref) => {
            //     *self.stack_value_mut(stack_ref) = self
            //         .value_stack
            //         .0
            //         .pop()
            //         .expect("value stack should not be empty");
            // }
            // Instruction::CopyInto(stack_ref) => {
            //     *self.stack_value_mut(stack_ref) = self
            //         .value_stack
            //         .0
            //         .last()
            //         .expect("value stack should not be empty")
            //         .clone();
            // }
            // Instruction::Swap(stack_ref) => {
            //     let a = self.stack_ref_index(stack_ref);
            //     let b = self.value_stack.0.len() - 1;
            //     self.value_stack.0.swap(a, b);
            // }
            // Instruction::Jump(instruction) => {
            //     jump_to_instruction = Some(instruction);
            // }
            // Instruction::JumpIf(instruction) => {
            //     if self.check_condition() {
            //         jump_to_instruction = Some(instruction);
            //     }
            // }
            // Instruction::JumpUnless(instruction) => {
            //     if !self.check_condition() {
            //         jump_to_instruction = Some(instruction);
            //     }
            // }
            // Instruction::Call => match self.value_stack.0.last() {
            //     Some(Value::Function(function)) => self.call_stack.0.push(Call {
            //         function: function.clone(),
            //         instruction_pointer: InstructionPointer(0),
            //     }),
            //     _ => panic!("stack value should be a function"),
            // },
            // Instruction::PopCall => match self.value_stack.0.pop() {
            //     Some(Value::Function(function)) => self.call_stack.0.push(Call {
            //         function,
            //         instruction_pointer: InstructionPointer(0),
            //     }),
            //     _ => panic!("stack value should be a function"),
            // },
            // Instruction::CallNative(function) => self = function(self),
            // Instruction::Return => {
            //     self.call_stack.0.pop();
            // }
            // Instruction::Yield(count) => {
            //     let call = self
            //         .call_stack
            //         .0
            //         .pop()
            //         .expect("call stack should not be empty");

            //     let value_stack = ValueStack(
            //         self.value_stack
            //             .0
            //             .drain((self.value_stack.0.len() - count - 1)..)
            //             .collect(),
            //     );

            //     self.value_stack
            //         .0
            //         .push(Value::Generator(Arc::new(Generator { call, value_stack })));
            // }
            // Instruction::Resume => match self.value_stack.0.pop() {
            //     Some(Value::Generator(generator)) => {
            //         // TODO: Use Arc::unwrap_or_clone once stable.
            //         let generator = Arc::try_unwrap(generator).unwrap_or_else(|arc| (*arc).clone());
            //         self.call_stack.0.push(generator.call);
            //         self.value_stack.0.extend(generator.value_stack.0);
            //     }
            //     _ => panic!("value stack should contain a generator"),
            // },
        }

        if let Some(jump_to_instruction) = jump_to_instruction {
            self.current_call_mut().instruction_pointer = jump_to_instruction;
        }

        StepResult::Running
    }

    fn check_condition(&self) -> bool {
        *self
            .value_stack
            .bits
            .last()
            .expect("stack should contain a bool")
    }
}

/// A compiled function, defined as a list of instructions.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Function {
    pub(crate) instructions: InstructionList,
}

impl Function {
    fn next_instruction(&self, instruction_pointer: &mut InstructionPointer) -> InstructionRef {
        self.instructions.next(instruction_pointer)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Generator {
    call: Call,
    values: Vec<Value>,
}

impl Function {
    fn halts_guaranteed(&self) -> bool {
        todo!()

        // TODO: Recursively check function calls.
        //       Keep track of called functions and deal with potential recursion.

        // self.0
        //     .iter()
        //     .enumerate()
        //     .all(|(instruction_pointer, instruction)| match instruction {
        //         InstructionRef::Push(..)
        //         | InstructionRef::Return
        //         | InstructionRef::Yield
        //         | InstructionRef::Nop => true,
        //         // InstructionRef::Jump(instruction)
        //         // | InstructionRef::JumpIf(instruction)
        //         // | InstructionRef::JumpUnless(instruction) => instruction.0 > instruction_pointer,
        //         InstructionRef::Call | InstructionRef::Resume => false,
        //     })
    }
}
