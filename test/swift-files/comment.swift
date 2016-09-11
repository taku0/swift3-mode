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
