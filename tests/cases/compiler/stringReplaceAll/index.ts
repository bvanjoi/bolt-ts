// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/stringPropCodeGen.ts`, Apache-2.0 License

//@compiler-options: target=es2021

let a = "abcde".replaceAll("hello");
//~^ ERROR: Expected 2 arguments, but got 1.
let b = "abcde".replaceAll("hello", "world");

{
  type P<T> = { [KeyType in keyof T as KeyType]: PP<T[KeyType]> };
  type PP<T> = T extends (...arguments_: any[]) => unknown ? T : P<{ [Key in keyof T]: never }>;
  type TestingType = {
    tuple: [{ propertyA: string }];
  };
  function f(bar: P<TestingType>) {
    const _a: TestingType = bar;
  }
}