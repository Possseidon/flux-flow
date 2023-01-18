use std::{mem::take, sync::Arc};

use super::{primitives::NativeFunction, Value, ValueStorage};

// TODO: Make sure the stack_frames shrinks_to_fit every now and then.
//       Limit its capacity to at least twice its current size.
//       -> On every return, if size() < 4 * capacity(), shrink_to()

// TODO: Recursion limit, by limiting the number of stack frames.
// TODO: Instruction for relative lookup table style jumps.

/// Comparable to a `main()` function.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ExecutionEnvironment {
    result: Value,
    stack_frames: Vec<StackFrame>,
    instruction_pointer: usize,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StackFrame {
    function: Arc<Function>,
    stack: Vec<Value>,
    /// Index to the function result, always inside the parent stack.
    result_stack_index: usize,
}

impl StackFrame {
    fn new(function: Arc<Function>, argument: Value, result_stack_index: usize) -> Self {
        Self {
            function,
            stack: vec![argument],
            result_stack_index,
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum StepResult {
    Running(ExecutionEnvironment),
    Done(Value),
}

impl ExecutionEnvironment {
    pub fn new(function: Arc<Function>, argument: Value) -> Self {
        Self {
            result: ().into(),
            stack_frames: vec![StackFrame::new(function, argument, 0)],
            instruction_pointer: 0,
        }
    }

    pub fn run(mut self) -> Value {
        loop {
            match self.step() {
                StepResult::Running(next) => {
                    self = next;
                }
                StepResult::Done(value) => break value,
            }
        }
    }

    pub fn step(self) -> StepResult {
        let instruction = self.next_instruction();
        self.execute_instruction(instruction)
    }

    fn current_stack_frame(&self) -> &StackFrame {
        self.stack_frames
            .last()
            .expect("at least one stack frame should always exist")
    }

    fn current_stack_frame_mut(&mut self) -> &mut StackFrame {
        self.stack_frames
            .last_mut()
            .expect("at least one stack frame should always exist")
    }

    fn current_function(&self) -> &Function {
        &self.current_stack_frame().function
    }

    fn next_instruction(&self) -> Instruction {
        let instructions = &self.current_function().instructions;
        instructions
            .get(self.instruction_pointer)
            .unwrap_or_else(|| {
                if self.instruction_pointer == instructions.len() {
                    &Instruction::Return
                } else {
                    panic!("invalid instruction pointer");
                }
            })
            .clone()
    }

    fn execute_instruction(mut self, instruction: Instruction) -> StepResult {
        let mut jump_to_instruction = None;

        match instruction {
            Instruction::Push { value } => {
                self.push(value);
            }
            Instruction::CopyInto { index } => {
                self.push(self.top(index).clone());
            }
            Instruction::Pop { count } => {
                for _ in 0..count {
                    self.pop();
                }
            }
            Instruction::PopInto { index } => {
                self.current_stack_frame_mut().stack[index] = self.pop();
            }
            Instruction::Swap { index } => {
                let stack = &mut self.current_stack_frame_mut().stack;
                let top = stack.len() - 1;
                stack.swap(index, top);
            }
            Instruction::Jump { instruction } => {
                jump_to_instruction = Some(instruction);
            }
            Instruction::JumpIf { instruction } => {
                if self.check_condition() {
                    jump_to_instruction = Some(instruction);
                }
            }
            Instruction::JumpUnless { instruction } => {
                if !self.check_condition() {
                    jump_to_instruction = Some(instruction);
                }
            }
            Instruction::Call => {
                let stack = &mut self.current_stack_frame_mut().stack;
                let argument = stack.pop().expect("function argument expected");
                let result_stack_index = stack.len() - 1;
                let function = take(stack.last_mut().expect("function expected"));
                self.call_function(function, argument, result_stack_index);
            }
            Instruction::CallResultAt { result_stack_index } => {
                let stack = &mut self.current_stack_frame_mut().stack;
                let argument = stack.pop().expect("function argument expected");
                let function = stack.pop().expect("function expected");
                self.call_function(function, argument, result_stack_index);
            }
            Instruction::CallNative { function } => {
                let stack = &mut self.current_stack_frame_mut().stack;
                let result_stack_index = stack.len() - 1;
                let argument = stack.pop().expect("function argument expected");
                self.call_native_function(function, argument, result_stack_index);
            }
            Instruction::Return => {
                let stack_frames = &mut self.stack_frames;
                stack_frames.pop();
                if stack_frames.is_empty() {
                    return StepResult::Done(self.result);
                }
                self.shrink_stack_frames();
            }
            Instruction::Yield => {
                todo!();
                // let stack_frame = self
                //     .stack_frames
                //     .pop()
                //     .expect("stack frames should not be empty");
                // let instruction_pointer = self.instruction_pointer + 1;
                // self.set_result(Value::Generator(Arc::new(Generator {
                //     stack_frame,
                //     instruction_pointer,
                // })));
            }
            Instruction::Resume => todo!(),
            Instruction::PopIntoResult => {
                let result = self
                    .current_stack_frame_mut()
                    .stack
                    .pop()
                    .expect("result expected");
                self.set_result(result);
            }
            Instruction::CopyIntoResult { from: index } => {
                self.set_result(self.current_stack_frame().stack[index].clone());
            }
            Instruction::Nop => {}
        }

        if let Some(jump_to_instruction) = jump_to_instruction {
            self.instruction_pointer = jump_to_instruction;
        } else {
            self.instruction_pointer += 1;
        }

        StepResult::Running(self)
    }

    fn call_function(&mut self, function: Value, argument: Value, result_stack_index: usize) {
        match function.0 {
            ValueStorage::Function(function) => {
                self.stack_frames.push(StackFrame {
                    function,
                    stack: vec![argument],
                    result_stack_index,
                });
            }
            ValueStorage::NativeFunction(function) => {
                self.call_native_function(function, argument, result_stack_index);
            }
            _ => panic!("function expected"),
        }
    }

    fn call_native_function(
        &mut self,
        function: NativeFunction,
        argument: Value,
        result_stack_index: usize,
    ) {
        let result = function(argument);
        self.current_stack_frame_mut().stack[result_stack_index] = result;
    }

    fn top(&self, index: usize) -> &Value {
        &self.current_stack_frame().stack[index]
    }

    fn push(&mut self, value: Value) {
        self.current_stack_frame_mut().stack.push(value);
    }

    fn pop(&mut self) -> Value {
        self.current_stack_frame_mut()
            .stack
            .pop()
            .expect("cannot pop from an empty stack")
    }

    fn check_condition(&self) -> bool {
        self.current_stack_frame()
            .stack
            .last()
            .and_then(|value| match value.0 {
                ValueStorage::Boolean(boolean) => Some(boolean),
                _ => None,
            })
            .expect("boolean expected")
    }

    fn set_result(&mut self, result: Value) {
        let stack_frame_count = self.stack_frames.len();
        if stack_frame_count > 1 {
            let result_stack_index = self.current_stack_frame().result_stack_index;
            self.stack_frames[stack_frame_count - 2].stack[result_stack_index] = result;
        } else {
            self.result = result;
        }
    }

    fn shrink_stack_frames(&mut self) {
        let stack_frames = &mut self.stack_frames;
        if stack_frames.len() < stack_frames.capacity() / 4 {
            stack_frames.shrink_to(stack_frames.len() * 2)
        }
    }
}

/// A function consisting of a single parameter, single return value.
///
/// Only having a single parameter might sound weird, but in the end, having multiple parameters is
/// the same as passing a tuple. This also opens up the possibility of named parameters by instead
/// using an anonymous struct or even combining the two.
///
/// If the instruction pointer is out of bounds, it is treated as a [`Instruction::Ret`].
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Function {
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instruction {
    /// Pushes a constant value on the stack.
    Push { value: Value },
    /// Pushes a copy of a value from the given index to the top of the stack.
    CopyInto { index: usize },
    /// Pops a given number of elements from the stack.
    Pop { count: usize },
    /// Pops the top stack index, moving its value to the given index.
    PopInto { index: usize },
    /// Swaps the top stack index with the given index.
    Swap { index: usize },

    /// Jumps to the given instruction.
    Jump { instruction: usize },
    /// Jumps to the given instruction if the top stack index is `true`.
    JumpIf { instruction: usize },
    /// Jumps to the given instruction if the top stack index is `false`.
    JumpUnless { instruction: usize },

    /// Creates a new stack frame from the top two stack indices and executes it.
    ///
    /// The result is pushed on the top of the stack afterwards.
    Call,
    /// Creates a new stack frame from the top two stack indices and executes it.
    ///
    /// The result replaces the given stack index.
    CallResultAt { result_stack_index: usize },
    /// Calls the given function, effectively replacing the value at the top of the stack.
    CallNative { function: NativeFunction },
    /// Destroys the current stack frame.
    ///
    /// This also happens automatically when the instruction pointer goes out of bounds.
    Return,

    /// Wraps the current stack frame in a generator and returns it.
    Yield,
    /// Puts a generator's stack frame back on the stack and resumes it.
    Resume,

    /// Pops the top stack index, moving it to the parent stack frame's result index.
    PopIntoResult,
    /// Copies the given stack index to the parent stack frame's result index.
    CopyIntoResult { from: usize },

    /// Does nothing.
    Nop,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Generator {
    stack_frame: StackFrame,
    instruction_pointer: usize,
}

impl Function {
    fn halts_guaranteed(&self) -> bool {
        // TODO: Recursively check function calls.

        self.instructions
            .iter()
            .enumerate()
            .all(|(instruction_pointer, instruction)| match instruction {
                Instruction::Push { .. }
                | Instruction::CopyInto { .. }
                | Instruction::Pop { .. }
                | Instruction::PopInto { .. }
                | Instruction::Swap { .. }
                | Instruction::Return
                | Instruction::PopIntoResult
                | Instruction::CopyIntoResult { .. }
                | Instruction::Nop => true,

                Instruction::Jump { instruction }
                | Instruction::JumpIf { instruction }
                | Instruction::JumpUnless { instruction } => *instruction > instruction_pointer,

                Instruction::Call
                | Instruction::CallResultAt { .. }
                | Instruction::CallNative { .. }
                | Instruction::Yield
                | Instruction::Resume => false,
            })
    }
}
