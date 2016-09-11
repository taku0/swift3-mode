// swift3-mode:test:eval (setq-local swift3-mode:basic-offset 4)
// swift3-mode:test:eval (setq-local swift3-mode:parenthesized-expression-offset 2)
// swift3-mode:test:eval (setq-local swift3-mode:multiline-statement-offset 2)
// swift3-mode:test:eval (setq-local swift3-mode:switch-case-offset 0)


@Annotation(aaa)
private
  /* */ final /*
               */ class /*
                         */ Foo /*
                                 */ {
    aaa() // swift3-mode:test:known-bug
    bbb()
} // swift3-mode:test:known-bug
