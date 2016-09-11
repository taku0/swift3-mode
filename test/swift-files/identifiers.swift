// swift3-mode:test:eval (setq-local swift3-mode:basic-offset 4)
// swift3-mode:test:eval (setq-local swift3-mode:parenthesized-expression-offset 2)
// swift3-mode:test:eval (setq-local swift3-mode:multiline-statement-offset 2)
// swift3-mode:test:eval (setq-local swift3-mode:switch-case-offset 0)


// Backquoted identifier must behave like normal identifier

enum `switch` {
  case 1 // swift3-mode:test:known-bug
}

do {
} catch `case`
          where a // swift3-mode:test:known-bug


let foo = `var`
  .then {
  } // swift3-mode:test:known-bug

let x = `where` +
  a // swift3-mode:test:known-bug


// Keywords after dot must behave like normal identifier
// https://github.com/apple/swift-evolution/blob/master/proposals/0071-member-keywords.md

let foo = foo.var
  .then {
  }

let x = foo.where +
  a

// Unicode identifiers

let こんにちは = 你好 +
  안녕하세요 +
  😊
