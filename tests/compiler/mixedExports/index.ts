// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mixedExports.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare namespace M {
     function foo();
     export function foo();
     function foo();
}

declare namespace M1 {
     export interface Foo {}
     interface Foo {}
}

namespace A {
     interface X {x}
     export namespace X {}
     interface X {y}
}