// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseJsxExtends1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict

export function Foo() {
  return <const  T extends />;
}