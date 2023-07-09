# Pattern Matching

**TODO: Make this pretty, this is me jotting down some notes!**

---

match and if let

they are magic in that

match x {
  A | B => x,
  C => A,
  _ => B,
}

This returns A | B. The moment the branch A | B matches, x's type is changed to A | B. In Rust this is not possible like this, as x keeps its old type and you would have to write it more like this:

match x {
    A => A,
    B => B,
    C => A,
    _ => B,
}

if let does the same thing:

if let A | B = x {
    // type of x changes to A | B
}

When matching `A | B` on `x` and `x` can never hold `B`, show an unmatchable pattern warning.

Additionally the inverse also applies. Once a branch mismatches, all of the types are removed from the set of possible types.

let x: A | B | C | D;
match x {
    A | B => x, // x is type A | B
    _ => x, // x is type C | D
}

again, same with else branches of an if-let chain
