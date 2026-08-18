// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportedBlockScopedDeclarations.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: module=esnext

const foo = foo; // compile error
//~^ ERROR: Block-scoped variable 'foo' used before its declaration.
export const bar = bar; // should be compile error
//~^ ERROR: Block-scoped variable 'bar' used before its declaration.
function f() {
  const bar = bar; // compile error
//~^ ERROR: Block-scoped variable 'bar' used before its declaration.
}
namespace NS {
  export const bar = bar; // should be compile error
//~^ ERROR: Block-scoped variable 'bar' used before its declaration.
}

let foo1 = foo1; // compile error
//~^ ERROR: Block-scoped variable 'foo1' used before its declaration.
export let bar1 = bar1; // should be compile error
//~^ ERROR: Block-scoped variable 'bar1' used before its declaration.
function f1() {
  let bar1 = bar1; // compile error
//~^ ERROR: Block-scoped variable 'bar1' used before its declaration.
}
namespace NS1 {
  export let bar1 = bar1; // should be compile error
//~^ ERROR: Block-scoped variable 'bar1' used before its declaration.
}