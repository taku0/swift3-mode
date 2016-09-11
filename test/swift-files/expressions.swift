// swift3-mode:test:eval (setq-local swift3-mode:basic-offset 4)
// swift3-mode:test:eval (setq-local swift3-mode:parenthesized-expression-offset 2)
// swift3-mode:test:eval (setq-local swift3-mode:multiline-statement-offset 2)
// swift3-mode:test:eval (setq-local swift3-mode:switch-case-offset 0)

// Prefix expressions
// See also operators.swift

foo(
  +a,
  +b,
  &x
)

// Try operators

let foo = try
  a() + try
  b()

let foo = try!
  a() + try!
  b()

let foo = try?
  a() + try?
  b()

let foo = a+try
  a() +b+try
  b()

let foo = a+try!
  a() + b+try!
  b()

let foo = a+try?
  a() + b+try?
  b()

// Binary expressions
// See also operators.swift

let foo = 1 +
  /* */ 2 +
  3 + 4 +
  /* */ 5

let foo = 1 +
  2 +
    3 + 4 + // swift3-mode:test:keep-indent
    5

// swift3-mode:test:eval (setq-local swift3-mode:multiline-statement-offset 4)

let foo = 1 +
    2 +
    3 + 4 +
    5

// swift3-mode:test:eval (setq-local swift3-mode:multiline-statement-offset 2)

// Ternary conditional operator

let foo = a()
  ? b()
  : c()

let foo = a() ?
  b() :
  c()

// Type-casting operators

let foo = a is
  A || b is
  B

let foo = a
  is A || b
  is B

let foo = a as
  A +++ b as
  B

let foo = a
  as A +++ b
  as B

let foo = a as?
  A +++ b as?
  B

let foo = a
  as? A +++ b
  as? B

let foo = a as!
  A +++ b as!
  B

let foo = a
  as! A +++ b
  as! B

// Literal expression

// Special literal

let foo =
  #file

let foo = #file
  +++ #function

if
  #file == a { // swift3-mode:test:known-bug
} // swift3-mode:test:known-bug
foo() // swift3-mode:test:known-bug

// Array literal

let x = [
  1,
  2,
  3, 4
  5
]

let x = [
  1,
  2,
  3, 4
  5,
]

let x =
  [
    1,
    2,
    3, 4
    5
  ]

let x = [ 1,
          2,
          3, 4
          5 ]

// Dictionary literal

let x = [
  aaa:
    aaa
    + aaa,
  aaa:
    aaa
    + aaa,
  aaa: aaa, aaa:
              aaa
              + aaa,
  aaa
    : aaa
    + aaa,
  aaa: aaa, aaa
              : aaa
              + aaa,
  aaa
    : aaa
    + aaa
]

let x =
  [
    aaa:
      aaa
      + aaa,
    aaa:
      aaa
      + aaa,
    aaa: aaa, aaa:
                aaa
                + aaa,
    aaa
      : aaa
      + aaa,
    aaa: aaa, aaa
                : aaa
                + aaa,
    aaa
      : aaa
      + aaa
  ]

let x = [ aaa:
            aaa
            + aaa,
          aaa:
            aaa
            + aaa,
          aaa: aaa, aaa:
                      aaa
                      + aaa,
          aaa:
            aaa
            + aaa
          aaa
            : aaa
            + aaa,
          aaa: aaa, aaa
                      : aaa
                      + aaa,
          aaa
            : aaa
            + aaa ]

let x = [
  :
]

let x =
  [
    :
  ]

let x = [ :
]

// Closure expressions

let x = {
    println("Hello, World!")
    println("Hello, World!")
}

let x = { x in
    println("Hello, World! " + x)
    println("Hello, World! " + x)
}

let x = { x
          in
    println("Hello, World! " + x)
    println("Hello, World! " + x)
}

let x = {
    x in
    println("Hello, World! " + x)
    println("Hello, World! " + x)
}

let x = {
    x
    in
    println("Hello, World! " + x)
    println("Hello, World! " + x)
}

let x = { (
            x: Int,
            y: Int
          )
            ->
            Foo
          in
    println("Hello, World! " + x + y)
    println("Hello, World! " + x + y)

    return foo
}

