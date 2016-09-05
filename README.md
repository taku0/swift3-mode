[![License GPL 3][badge-license]][copying]

# swift3-mode

Major-mode for Apple's Swift programming language.

This is a fork of [chrisbarrett/swift-mode](https://github.com/chrisbarrett/swift-mode) with a new indentation engine, supporting Swift 3.

## Installation

Download [latest release](https://github.com/taku0/swift3-mode/releases) and execute `M-x package-install-file`.

## Features

- Font Lock
- Indentation

  ```swift
  // Common-style
  while
    case let .P1(x),
         let .P2(x) = xs,
    case let .P3(y),
         let .P4(y) = ys {
      foo()
  }

  // Utrecht-style
  while case let .P1(x)
           , let .P2(x)
               = xs
      , case let .P3(y)
           , let .P4(y)
               = ys
  {
      foo()
  }
  ```
- `forward-sexp`
- `beginning-of-defun` and `end-of-defun`
- `indent-new-comment-line`
- Imenu

This package does not provide REPL and flycheck. Those should be separate packages.

## Limitations

Some syntax constructs removed from Swift 3.0 are not supported:

- C-style for-loop: `for var i = 1; i < 10; i++ { }`
- Multiple assignments in single `if let`:
  ```swift
  if let x = x,
         y = y {
  }
  ```

  Use multiple `let` instead:
  ```swift
  if let x = x,
     let y = y {
  }
  ```

Indentation may not accurate. For example, `foo(Bar < A, B > (c))` is ambiguous. It is indented like either
```swift
foo(Bar < A,
    B > (c)) // Passing two Boolean arguments to foo
```
or
```swift
foo(Bar < A,
          B > (c)) // constructing Bar with two type arguments and a value
```
The Swift compiler disambiguates this case using tokens after `>`, but those tokens may not available in editing time. We use some heuristic for this.

Another example is difficulty of handling of colons. We have to pair all `?` and `:` of conditional operators to decide indentation. This is a future work.

```swift
switch foo {
  case let P(x) where x is Foo? ? a ? b : c ?? d : e ? f : g : h ? i?.j() : k()
}

switch foo {
  case let P(x) where (x is Foo?) ? (a ? b : c ?? d) : (e ? f : g) :
    h ? i?.j() : k()
}
```

Yet another difficult case is consistency of blocks. We want to indent method chains like this:
```swift
var x = foo
  .then { x in
      aaa
  }
  .then { x in
      aaa
  }
```

while we also want to indent if body like this:

```swift
if anotherVeryLongVariableName
     .veryLongPropertyName {
    aaa
}
```

Then, how should we indent this when the cursor is before `@`?

```swift
var x = foo
  .bar {
    @
```

This could be
```swift
var x = foo
  .bar {
    @abc willSet {
        aaa
    }
}
```
or
```swift
var x = foo
  .bar {
      @abc var x = 1
      x
  }
```

Both are syntactically correct code. We cannot handle this case properly. This is also a future work.

## Related projects

- [Official swift-mode.el by Apple](https://github.com/apple/swift/blob/master/utils/swift-mode.el) Seems still in very early stage for now. We cannot contribute to it due to the license incompatibility.

## License

GPLv3. See [COPYING][] for details. Copyright (C) 2014-2016 taku0, Chris Barrett, Bozhidar Batsov, Arthur Evstifeev.

[badge-license]: https://img.shields.io/badge/license-GPL_3-green.svg
[COPYING]: ./COPYING
