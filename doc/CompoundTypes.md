# Compound Types and Literals

| Style | Description          | Types                         |
| ----- | -------------------- | ----------------------------- |
| `()`  | heterogeneous        | **Tuple**, **Struct**         |
| `[]`  | homogeneous, ordered | **Array**, **Vec**, **Deque** |
| `{}`  | homogeneous, sorted  | **Set**, **Map**              |
| `#`   | indexed              | **Tuple**, **Set**            |
| `@`   | key-value            | **Struct**, **Map**           |

Array literals are not prefixed with `#` as `#[]` is already used for attributes.

| Meaning                    | Type                 | Literal            | Pattern         |
| -------------------------- | -------------------- | ------------------ | --------------- |
| **Tuple**                  | `#(i32, bool)`       | `#(1, true)`       | `#(a, b)`       |
| **Tuple** (explicit index) | `#(0: i32, 1: bool)` | `#(0: 1, 1: true)` | `#(0: a, 1: b)` |
| **Struct**                 | `@(a: i32, b: bool)` | `@(a: 1, b: bool)` | `@(a: a, b: b)` |
| **Struct** (implicit name) | *n/a*                | `@(a, b)`          | `@(a, b)`       |
| **Array**                  | `[2]i32`             | `[1, 2]`           | `[a, b]`        |
| **Vec**                    | `[]i32`              | from array         | `[a, b]`        |
| **Deque**                  | `[~]i32`             | from array         | `[a, b]`        |
| **Set**                    | `[<]i32`             | `#{1, 2}`          | `#{a, b}`       |
| **Map**                    | `[i32]bool`          | `@{1: true}`       | `@{a: b}`       |

Collection types use prefix notation for the indexing scheme as it brings quite a few benefits over classic suffix notation:

- `[X]Y` can be read from left to right as "when indexed by `X` it returns `Y`"
  - It can kindof be read like a function signature: `[X] -> Y`
- Nested array indexing has the same order as specified in the type:
  - `mat: [N][M]i32` when indexed becomes `mat N M`

To disambiguate the use of a type in an expression, it has to be prefixed by `type`:

```flx
// A tuple of the types i32 and bool
let t = #(i32, bool);

// The type of a tuple holding an i32 and bool
let t = type #(i32, bool);
```
