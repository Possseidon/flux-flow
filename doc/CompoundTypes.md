# Compound Types

Also known as product types, as the possible values of e.g. an array of two `u8`s (`[u8; 2]`) are all possible values of one `u8` multiplied by all the possible values of another `u8` for a total of 256 * 256 = 65536 possible values.

| Types              | Element Types | Element Order | Size     |
| ------------------ | ------------- | ------------- | -------- |
| **Struct**         | heterogeneous | ordered       | fixed    |
| *n/a*              | heterogeneous | sorted        | fixed    |
| doesn't make sense | heterogeneous |               | variable |
| **Array**          | homogeneous   | ordered       | fixed    |
| **Vec**, **Deque** | homogeneous   | ordered       | variable |
| *n/a*              | homogeneous   | sorted        | fixed    |
| **Set**, **Map**   | homogeneous   | sorted        | variable |

It does not make sense to have a variable size heterogeneous type.

| Meaning         | Type                | Literal           | Pattern        | Storage |
| --------------- | ------------------- | ----------------- | -------------- | ------- |
| **Struct**      | `(a: i32, b: bool)` | `(a: 1, b: true)` | `(a: a, b: b)` | Stack   |
| (implicit name) |                     | `(:a, :b)`        | `(:a, :b)`     |         |
| **Array**       | `[2]i32`            | `[1, 2]`          | `[a, b]`       | Stack   |
| **Vec**         | `[]i32`             | *n/a*             | `[a, b]`       | Heap    |
| **Deque**       | `[~]i32`            | *n/a*             | `[a, b]`       | Heap    |
| **Set**         | `[<]i32`            | `#{1, 2}`         | `#{a, b}`      | Heap    |
| **Map**         | `[i32]bool`         | `@{1: true}`      | `@{a: b}`      | Heap    |

Collection types use prefix notation for the indexing scheme as it brings quite a few benefits over classic suffix notation:

- `[X]Y` can be read from left to right as "when indexed by `X` it returns `Y`"
  - It can kindof be read like a function signature: `[X] -> Y`
- Nested array indexing has the same order as specified in the type:
  - `mat: [N][M]i32` when indexed becomes `mat N M`

## Disambiguations

### TypeSet

The empty **TypeSet** can be written as `unit`.

Every type/value is a **TypeSet**. There is no difference between `i32` and `i32,`. Parentheses can be used to group them however:

### Groups vs Structs

Expression groups and structs both use parentheses, but their syntax still makes it obvious which one was meant.

It's a struct if it either:

- Has an immediate closing parenthesis: `()`
- Has an immediate colon: `(:a)`
- Has an immediate ident followed by a colon: `(a: 42)`

If none of these rules apply, it's an expression group.

### Types vs Expressions

To disambiguate the use of a type in an expression, it has to be prefixed by `type`:

```flx
// A struct where x holds the type i32
let t = (x: i32);

// The type of a struct with a field x of type i32
let t = type (x: i32);
```

## Tuples

There are no tuples and this is by design.

Tuples just aren't necessary. At some point you're bound to unpack the tuple and give its fields names, so why not just use a proper struct with named fields to begin with.

> But I want to store my version numbers in a tuple of three numbers!

Then either use a struct and give proper names or use a fixed size array:

```flx
type Version: (
  major: u32,
  minor: u32,
  patch: u32,
);`

type Version: [3]u32;
```

Both compile down to basically the same thing.
