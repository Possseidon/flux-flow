
// impl Fn(usize) -> i32 for []i32 {
//   fn call self index: usize -> i32 { }
// }

// impl i32[] {
//   fn push self #(index: usize, value: i32) -> Self { }
//   fn update self #(index: usize, value: i32) -> Self { }
// }

fn test _ {
  let add = |#(lhs, rhs)| lhs + rhs;
  let add_1_2 = add.#(1, 2);

  let x = add_1_2 #();

  let y = add @(lhs: 1); // error, rhs expected
  let z = add.@(lhs: 1);
  // let a: []i32;

  // a = []

  let b = a
    .push 1;

  // a = []
  // b = [1]

  let c = b
    .=push 2
    .push 3;

  // a = []
  // b = [1, 2]
  // c = [1, 2, 3]

  let d = c
    .push 4
    .=push; 5; // Warning: `.=` on temporary does not have any effect

//   a = []
//   b = [1, 2]
//   c = [1, 2, 3]
//   d = [1, 2, 3, 4, 5]

  // ~ calls still return self to allow for further chaining:

//   _ = values.call 0;
//   _ = values 0;

  values = values.update #(0, 256);
  values = values.[0: 256];

  values.=update #(0, 256);
  values.=[0: 256];
  let other = values.[0: 256];

  let person = @{
    name: 234,
    age: 42,
  };

  person = person.@{ name: asd };
  person.=@{ name: blub };

  x = x.BLA;
  x.=BLA;

  x = x + BLA;
  x += BLA;

  let a = #(1, 2, 3);
  let b = a.#(1: 4);
  a.=#(1: 4);
  let b = a.#(1: 4);

  a.b = 42;

  let a = @(x: 1, y: 2, z: 3);
  let b = a.@(y: 4);
  a.=@(
    y: 4,
    z: 5,
  );
}
