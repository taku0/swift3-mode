// swift3-mode:test:eval (setq-local swift3-mode:basic-offset 4)
// swift3-mode:test:eval (setq-local swift3-mode:parenthesized-expression-offset 2)
// swift3-mode:test:eval (setq-local swift3-mode:multiline-statement-offset 2)
// swift3-mode:test:eval (setq-local swift3-mode:switch-case-offset 0)

// Simple types

let foo: A
  = abc

let foo:
  A = abc

let foo
  :A = abc

class Foo:
  A,
  B, C,
  D {
}

class Foo
  : A,
    B, C,
    D {
}


class Foo: A
         , B , C
         , D
{
}

class Foo
  : A
  , B , C
  , D
{
}


// Types with attribute

let foo: @A A
  = abc

let foo: @A
  A = abc // swift3-mode:test:known-bug
foo() // swift3-mode:test:known-bug

let foo:
  @A
  A = abc // swift3-mode:test:known-bug
foo() // swift3-mode:test:known-bug

let foo
  :@A
  A = abc // swift3-mode:test:known-bug
foo() // swift3-mode:test:known-bug

class Foo:
  @A
  A, // swift3-mode:test:known-bug
  B { 
} // swift3-mode:test:known-bug
foo() // swift3-mode:test:known-bug

class Foo
  : @A
    A, // swift3-mode:test:known-bug
    B {
} // swift3-mode:test:known-bug
foo() // swift3-mode:test:known-bug

class Foo: @A
           A // swift3-mode:test:known-bug
         , B // swift3-mode:test:known-bug
{ // swift3-mode:test:known-bug
}

class Foo
  : @A
    A // swift3-mode:test:known-bug
  , B // swift3-mode:test:known-bug
{ // swift3-mode:test:known-bug
}

// Member types

let foo:
  /* */ A.
  /* */ B = abc

let foo:
  /* */ A
  /* */ .B = abc

class Foo:
  A.
    B, // swift3-mode:test:known-bug
  A.
    B,
  A
    .B {
}

class Foo
  : A.
      B, // swift3-mode:test:known-bug
    A.
      B,
    A
      .B {
}

class Foo: A.
             B, // swift3-mode:test:known-bug
         , A.
             B
         , A
             .B
{
}

class Foo
  : A.
      B // swift3-mode:test:known-bug
  , A.
      B,
  , A
      .B {
}

// Array types

let foo: [
  A
]
  = abc

let foo:
  [
    A
  ] = abc

let foo
  :[
    A
  ] = abc

// Tuple types

let foo: (
  /* */ A,
  B
)
  = abc

let foo:
  (
    /* */ A,
    B
  ) = abc

let foo
  :(
    /* */ A,
    B
  ) = abc

// Dictionary types

let foo: [
  /* */ A:
    B
]
  = abc

let foo:
  [
    /* */ A:
      B
  ] = abc

let foo
  :[
    /* */ A:
      B
  ] = abc

// Function types

let foo: (
  A,
  B
)
  ->
  throws (
    A,
    B
  )
  ->
  throws
  [
    A
  ]
  = abc


let foo:
  (
    A,
    B
  )
  ->
  throws
  (
    A,
    B
  )
  ->
  throws
  [
    B
  ]
  = abc

let foo
  :(
    A,
    B
  )
  ->
  throws
  B
  = abc

let foo:
  (A, B)
  ->
  rethrows
  B
  = abc

let foo
  :(A, B)
  ->
  rethrows
  B
  = abc


// Optional types

let foo: A?
  = abc

let foo:
  A? = abc

let foo: A!
  = abc

let foo:
  A! = abc

// Protocol composition types

let foo: protocol<A<[B]>,
                  C<(D, E)>>
  = a

let foo: protocol<
  A, // swift3-mode:test:known-bug
  B
> // swift3-mode:test:known-bug
  = a
