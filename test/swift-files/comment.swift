// swift3-mode:test:eval (setq-local swift3-mode:basic-offset 4)
// swift3-mode:test:eval (setq-local swift3-mode:parenthesized-expression-offset 2)
// swift3-mode:test:eval (setq-local swift3-mode:multiline-statement-offset 2)
// swift3-mode:test:eval (setq-local swift3-mode:switch-case-offset 0)

// aaa
// bbb
// ccc
/*
 * aa
 * aa
 * aa
 */

/* */ class Foo {
    // aaa
    // bbb
      // ccc // swift3-mode:test:keep-indent
      // ddd // swift3-mode:test:known-bug
      /* // swift3-mode:test:known-bug
       * aa
         * aa // swift3-mode:test:keep-indent
         * aa
         */
}
