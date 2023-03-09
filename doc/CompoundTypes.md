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
| **Array**                  | `i32[2]`             | `[1, 2]`           | `[a, b]`        |
| **Vec**                    | `i32[]`              | from array         | `[a, b]`        |
| **Deque**                  | `i32[~]`             | from array         | `[a, b]`        |
| **Set**                    | `i32[<]`             | `#{1, 2}`          | `#{a, b}`       |
| **Map**                    | `bool[i32]`          | `@{1: true}`       | `@{a: b}`       |

To disambiguate the use of a type in an expression, it has to be prefixed by `type`:

```flx
// A tuple of the types i32 and bool
let t = #(i32, bool);

// The type of a tuple holding an i32 and bool
let t = type #(i32, bool);
```
