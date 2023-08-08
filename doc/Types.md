# Types

There is a whole bunch of built-in types.

## Boolean

True, False, Maybe

## Number

There are three types of numbers that are compatible with each other:

- Integers
- Rational Numbers
- Real Numbers

### Integers

| Type                        | Does the Integer `i` fit? |
| --------------------------- | ------------------------- |
| `std::num::Integer`         | `true`                    |
| `std::num::Natural`         | `i >= 0`                  |
| `std::num::PositiveInteger` | `i > 0`                   |
| `std::num::NegativeInteger` | `i < 0`                   |

A type that can hold any integer. The internal storage will seamlessly switch to a `BigInt` type when necessary.

#### Bounded Integers

| Type    | Does the Integer `i` fit? |
| ------- | ------------------------- |
| `3`     | `i == 3`                  |
| `3..=7` | `i >= 3 && i <= 7`        |
| `3..7`  | `i >= 3 && i < 7`         |
| `3..`   | `i >= 3`                  |
| `..3`   | `i < 3`                   |
| `..=3`  | `i <= 3`                  |

Integers can have compile-time fixed bounds which propagate through mathmatical operations.

```rs
1 + 2; // type 3

let a: 0..=10;
let b: 0..=15;

a + b; // type 0..=25

a + 5; // type 5..=15
```

### Rational

0.5 -> 5 / 10 -> 1/2

2/3

### Real

### Units

Imaginary numbers are just numbers with the unit `std::num::complex::i`.

## Range

## String & Char

### Char

### String

## UUID

## Instant

## Collections

### List

### Set

### Map

## Function

## Struct

## NewType

## Trait
