# NewTypes

NewTypes, also known as "strong types", allow for the creation of types that are distinct from each other, even if their underlying type is the same.

They are extremely useful for type safety. Take this example:

```rs
type Meter: f32;
type Seconds: f32;
type Point: (x: f32, y: f32);
```

Both `Meter` and `Seconds` are distinct types, even if they are both just a `f32` under the hood. While it is easy to convert between a NewType and its underlying type (given the wrapped value is accessible, i.e. `pub`), you have to do so explicitly. Trying to pass a `Meter` or plain `f32` to a function that actually expects a `Seconds` would result in a compile error.

To create a new value, the type itself doubles as a function that takes the wrapped value as a parameter and returns the wrapped value:

```rs
let distance = Meter 42.0;
let duration = Seconds 0.5;
let center = Point (x: 1.0, y: 2.0);
```

Unwrapping the value can be done in a few different ways, including the `*` operator, as well as just using pattern matching.

```rs
let value = *distance;
let Meter value = distance;
let _ value = distance;

let (x, y) = *center;
let Point (x, y) = center;
let _ (x, y) = center;

// or even just, which automatically unwraps
let (x, y) = center;

// or using field access
let x = center.x;
// automatically unwraps as if doing:
let x = (*center).x;
```

**TODO: Make sure the `_` syntax can be implemented and update the example if it doesn't. Actually I'm not even sure yet how to parse pattern matching like here at all.**

Note that for both wrapping and unwrapping, the field in question must be accessible (i.e. `pub`).

**TODO: Does it make sense to have separate access for get/set? Might be useful for e.g. a public constructor, while disallowing reading the field. Actually, a private constructor with public fields can already be achieved by simply adding a private dummy field. (Annoying to use though.) I do think it's a common pattern to have a public `new` function initialization all members though, so it might make sense after all.**

**TODO: Do I even want per field accessibility specifiers? It would simplify quite a bit, if the only way was `type Point: pub (x: f32, y: f32)`. Access specifiers don't make sense for anonymous structs after all. This `pub` then simply allow the use of `Point` as a constructor and direct field access including pattern matching.**

## Named Structs and Enums

Both named structs and enums can be defined using the `type X: ...` syntax. Note the `:` which differentiates it from alias types, which uses `=` instead of a `:`, as in `type X = ...`.

```rs
type Point: (
    x: f32,
    y: f32,
);
```

While structs only use a slightly different syntax when compared to Rust structs, enums are a bit more complicated to implement by hand:

```rs
type Link: Self::Localhost | Self::Url | Self::YouTube | Self::GitHubRepo;

impl Link {
    type Localhost;
    type Url: String;
    type YouTube: (id: String);
    type GitHubRepo: (owner: String, repo: String);
}
```

It makes use of nested `type` declarations inside the `impl` block. This allows referring to the type using e.g. `Link::Url`.

While this is a bit different from Rust, it fits quite well into the language and allows for more flexibility. For example, you can create a second enum that uses a subset of one or more other enums or even completely different types:

```rs
type Gray: u8;
struct RGBA { r: u8, g: u8, b: u8, a: u8 }

type Color: Gray | RGBA | Self::Hex;

impl Color {
    type Hex: u32;
}
```

This might be a bit of an odd example, but it showcases, how you can mix both nested `type`s, as well as completely separate types that might even be part of an entirely different module. A more common use-case for combining enums in such a way might be for combining various error types into a single type.

## Syntax Sugar

Both of them come with syntactic sugar, which, in the case of structs, makes them look basically the same as in Rust:

```rs
struct Point {
    x: f32,
    y: f32,
}
```

Enums look slightly different than in Rust. Note the `:` syntax! This syntax does cut down on boilerplate and duplication which is tedious and error-prone:

```rs
enum Link {
    Localhost,
    Url: String,
    YouTube: (id: String),
    GitHubRepo: (owner: String, repo: String),
}
```

### Named Unit Structs

When defining a NewType, the actual type can be left empty like so:

```rs
type Point;
```

This is indeed valid and is simply syntactic sugar for wrapping a unit struct:

```rs
type Point: ();
// or
struct Point {}
```

This is especially common for enums, as enums don't always need actual data inside their variants.

### Never Enums

An empty `enum` can never be constructed, which is why it is similar to the never type. In fact, the never type itself is basically just an empty anonymous sum type.

```rs
enum Never {}
// is the same as
type Never: !;
```

It is a distinct type from the actual never type `!`, but shares some of its properties.

**TODO: Personally I cannot think of any practical use like at all, but at least in theory it does make sense. I am however not sure how exactly this should be implemented. The never type `!` has the special property, that it is compatible with any other type. That's basically it's main use. But why would you need a NewType wrapping it then? That completly defeats the purpose. Probably needs some more thinking.**
