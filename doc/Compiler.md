# Compiler

**TODO**:

- Generate Attributes from doc-comments
- Ignore syntax tree nodes with inactive conditional compilation attributes
- Generics will probably need some more work

**Implementation Note:**

Often times, e.g. when parsing literals, there is no need to traverse the `SyntaxTree` properly. `SyntaxTree` conveniently already holds all items of the same type in separate lists, so one can just create a new list that can be indexed with the same indices as the original list.

## Lexer & Parser

`ModuleSourceFile` + `TokenStream` > `SyntaxTree`

## Interface Generation

`SyntaxTree` > `ModuleInterface`

- Always use full interface even if public were sufficient (for better error messages)
- Contains
  - `use`
  - `fn`
  - `type` (structs, enums, aliases)
  - `trait`
  - `impl` including trait `impl`

## Path Resolution

**in**:

- `SyntaxTree`
- All `ModuleInterface`s of source files being compiled
- All `ModuleInterface`s of other crates, including builtin preludes

**out**: `ResolvedPaths`

## Literal Parser

`SyntaxTree` > `ParsedLiterals`

- Numbers are not parsed into concrete types yet as the type depends on type inferred usage.

## Operator Desugaring

`SyntaxTree` > `DesugaredOperators`

- Turn `a + b` into plain function calls `add(a, b)` while considering their predence
- An operator chain `a + b * c + d` turns into `add(add(a, mul(b, c)), d)`
- `add` and `mul` here are merely for brevity and actually resolve to the corresponding trait functions
- This does not require `ResolvedPath` as the trait functions are implemented internally anyway

## Flow Analysis

`SyntaxTree` + `DesugaredOperators` > `FlowGraph`

- Creates a flow graph for each function block
- Output should contain a list of a mix of
  - Variable declarations
  - Function calls
  - Jumps

**TODO**:

I think the flow graph should be apparent without knowing any concrete types, unless I need to know if something diverges (never type) at this stage. While `return` statements obviously diverge, a function might do so as well, which we cannot know without resolving functions. Anyway, it probably makes sense to do that in a separate step.

## Type Inference

`FlowGraph` + `ResolvedPaths` > `InferredTypes`

## Number Literal Inference

`InferredTypes` > `ParsedNumbers` + `NumberParseErrors`

Now that types have been inferred, we can finally parse numbers into their concrete type.

## Type Checking

`FlowGraph` + `ResolvedPaths` + `InferredTypes` > `TypeErrors`

## Code Optimization

`FlowGraph` > `FlowGraph`

All steps are optional.

- Constant Propagation
- Copy Elimination
- Dead Code Elimination
- Function Inlining
- Tail Call Elimination

## Code Generation

`FlowGraph` > `InstructionList`
