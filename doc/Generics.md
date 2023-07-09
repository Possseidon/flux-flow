# Generics

Types and functions can have a single generic parameter. This parameter however can be of any type. In fact, *every* type has a single generic parameter. But it's usually just the unit struct `()` and cleanly hidden away by syntactic sugar. This generic parameter is always implicitly `const`, which means, it must be known at compile time.

## Types and Values

Values can be used as types and types can be used as values.

### Values as Types

When using a type of a value, possibel set of values for that type is only the value itself.

```rs
let one: 1 = 1; // one is of type 1 and holds the value 1
let two: 2 = 1; // compile time error; two of type 2 can not hold value 1
```

Multiple values can be composed using sum types:

```rs
let x: 1 | 2 | 3; // x can hold any of 1, 2 or 3
```

And a lot of the built-in types can be seen as one gigantic sum type:

```rs
let x: u8;
// is kindof just
let x: 0 | 1 | 2 | ... | 255;

let x: bool;
// is pretty much just
let x: true | false;
```

Where it gets more interesting, is with types with an infinite number of possible values. For example a `String` can be arbirarily large:

```rs
let x: String;
// is hard to express, but something along the lines of this
let x: "" | "a" | "b" | ... | "z" | "aa" | "ab" | ... | "zzz...";
// except not just a to z but with all valid characters
```

### Types as Values

Any type can also be used as a value. This is commonly used for generic parameters, but it can just as well be used for runtime values.

```rs
let x: i32 | f32 = i32;
// x either holds the type i32 or the type f32

let x = i32 | f32;
// x holds the type "i32 | f32"
```

### Naming Conventions

Types are `CamelCase`, variables and functions are `snake_case` and `const` are `SCREAMING_SNAKE_CASE`. Types themselves however are in a sense just `const` types.

Since generic parameters are actually just `const`, they also fall into the `SCREAMING_SNAKE_CASE` naming convention.

## Generic Functions

TODO

## Generic Types

```rs
type Value;
// is actually syntactic sugar for
type Value<_: ()>;
```

A single generic parameter, that is simply the unit struct.

Let's instead take a look at a fixed size array type:

```rs
type IntArray<LENGTH: usize>: [LENGTH]i32;
```

This time, the generic parameter is not only named, but also of a type `usize`. This means we can use it like so:

```rs
let array = IntArray::<3> [1, 2, 3];
// or let the compiler infer the generic parameter
let array = IntArray [1, 2, 3];
```

But it's actually far more common to have generic *type* parameters. So let's change this example around and have not the size of the array generic, but its element type.

```rs
type Array3<T: type>: [3]T;
// or just
type Array3<T>: [3]T;
```

The `T` here is of type `type`, which means it can hold any type. Not a value of any type, but the type itself. Usage would then look like so:

```rs
let array = Array3::<i32> [1, 2, 3];
// or, inferred again
let array = Array3 [1, 2, 3];
```

It is important, that the value can be computed at compile time. That's why only types and `const` can be used:

```rs
const LENGTH: usize = 3;
let array = Array::<LENGTH> [1, 2, 3];

// compiler error, because length is not `const`
let length = 3;
let array = Array::<length> [1, 2, 3];
```

There might only be a single generic parameter, but since it can be of any type, nothing stops you from using a struct, so that's not actually a limitation:

```rs
// PARAMS is a struct holding a type T and a usize LEN
type Array<PARAMS: (T: type, LEN: usize)>: [PARAMS.LEN]PARAMS.T;
// or more concise, using pattern matching
type Array<(T: type, LEN: usize)>: [LEN]T;

// T and LEN must be specified explicitly
let array = Array<(T: i32, LEN: 3)> [1, 2, 3];
```

## Trait Bounds

Since FluxFlow is type checked, you can't do a whole lot with a generic parameter that could be literally any type. You could instead limit your generic parameter to "only works with i32 and i16 and i8" using a sum type, but that's not all that convenient.

Instead, this is where trait bounds come in. By limiting the generic parameter to a specific set of traits up front, any of those traits can then be used:

```rs
trait Printable {
    fn print(self);
}

fn print_all<T: Printable> items: []T {
    for item in items {
        itme.print();
    }
}
```

By limiting the generic parameter to only those types that implement `Printable`, the `print` method can be used in `print_all`.

Using `Printable` simply means, `T` must be in the set of types that implement that trait. It can be further composed like any other sum type:

```rs
fn print_all<PRINTABLE_OR_STRING: Printable | String> items: []PRINTABLE_OR_STRING {
    // PRINTABLE_OR_STRING is either any type that is printable or it is a fixed string
    // this means, either all elements are printable or all elements are that fixed string
    match PRINTABLE_OR_STRING {
        Printable => for item in items {
            item.print();
        }
        // if String itself implements Printable, this should rightfully show an unreachable warning
        String => PRINTABLE_OR_STRING.foo(),
    }
}

// if i32 doesn't implement Printable, this raises a compile time error
print_all::<i32> [1, 2, 3]

// if "hello" implements Printable, all three "hello" are printed
// otherwise "hello".foo() is called once
print_all::<"hello"> ["hello", "hello", "hello"]
```
