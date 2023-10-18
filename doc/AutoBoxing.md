# Auto-Boxing

By default, structs and fixed size arrays are passed value by value on the stack. This is generally very fast, except for when the struct/array in question is very big. In that case, it is often times more beneficial to move the value on the heap and take the burden of an additional allocation and indirection, which makes copies and moves much cheaper.

Additionally, this kind of boxing becomes necessary when dealing with recursive data structures which would have infinite size without any means of indirection.

While this could have been implemented very easily by simply forcing the developer to stick `box` wherever it makes sense, most of the time it is probably better to just let the compiler figure out, if a structure is big/recursive and should/has to be boxed. This then leads into the two separate cases of auto boxing:

## Large Structs/Arrays

Implementing auto boxing for structs and arrays is fairly straight forward. Pick a configurable global maximum size you want your structs to have before they get boxed, and you're good to go.

## Breaking Recursion

Recursive data structures can only exist through the means of breaking the recursion at some point through a sum type, like the `None` variant of a `T?`. You simply cannot instantiate a type that unconditionally recurses onto itself.

If we take a look at the most basic form of a linked list (albeit without any stored data):

```rs
type Node: (next: Node?);
```

...then, the only sensible thing here would be to box the `next` node. But take this slightly more complex example instead:

```rs
type A: (b: B?);
type B: (a: A?);
```

Where do you break the recursion? On `A` or `B`? The bigger one? But here they're the same size! Maybe just both then? But one would be sufficient!
