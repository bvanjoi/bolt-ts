// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsAndParametersTypeAliases2.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

// unused
type handler1 = () => void;
//~^ ERROR: 'handler1' is declared but never used.

function foo() {
//~^ ERROR: 'foo' is declared but its value is never read.
    type handler2 = () => void;
//~^ ERROR: 'handler2' is declared but never used.
    foo();
}

export {}

type A<T> = T extends number ? A<T> : never;
//~^ ERROR: 'A' is declared but never used.

interface B<T> {
//~^ ERROR: 'B' is declared but never used.
  C: T extends number ? B<T> : never;
}

namespace E {
  //~^ ERROR: 'E' is declared but its value is never read.
  E;
}