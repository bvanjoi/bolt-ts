// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/ambientClassMergesOverloadsWithInterface.ts`, Apache-2.0 License

declare class C {
  baz(): any;
  foo(n: number): any;
}
interface C {
  foo(n: number): any;
  bar(): any;
}
