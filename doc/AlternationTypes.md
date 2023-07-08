# Alternation Types

Also known as sum types, as the possible values of e.g. a `u8 | u16` are all possible values of a `u8` in addition to all the possible values of a `u16`.

## Optional

Often times you want to encode the possibility of the absence of a value into a type.

This can be done like so: `u32 | !u32`

It is necessary to encoded the original type in the `!` variant, so that nesting is possible. Imagine a function `pop` on a vector that returns `!` in case of an empty vector. If the vector itself already contains optional values, the return value of said `pop` function somehow has to differentiate between an empty vector and popping an existing `!` value:

```flx
// Example using some imaginary `Not` type
let vec = Vec::<i32 | Not>::new();
// Without a typed `Not`, the two different `Not`s merge together
let _: i32 | Not | Not = vec.pop();
// We don't know if `Not` meant "the vector is empty" or "a `Not` value was popped"
```

With a typed `!` type, this is no longer an issue:

```flx
let vec = Vec::<i32 | !i32>::new();
// Without a typed `Not`, the two different `Not` merge together
let _: i32 | !i32 | !!i32 = vec.=pop();
// Now we have three distinct variants for the cases:
// `i32`: a `i32` value was popped
// `!i32`: a `!i32` value was popped
// `!!i32`: the vector is empty and neither a `i32` nor a `!i32` value could be popped
```

Since this is such a common pattern, a `?` can be used instead:

```flx
let vec = Vec::<i32?>::new();
let _: i32?? = vec.=pop();
```

Basically, a `T?` gets expanded to `T | !T`, which can also be applied recursively and then leads to the following expansion:

```txt
T??
(T | !T)?
(T | !T) | !(T | !T)
(T | !T) | (!T | !!T)
T | !T | !T | !!T
T | !T | !!T
```

Note, that `!(A | B)` can be read as "neither A nor B" and thus is the same as `!A | !B`. However, despite looking like a double negative, `!!T` is a distinct type from `T`.

The same expansion can also be achieved by distributing the `?` instead of the `!`:

```txt
T??
(T | !T)?
T? | (!T)?
(T | !T) | (!T | !!T)
T | !T | !T | !!T
T | !T | !!T
```

## Never
