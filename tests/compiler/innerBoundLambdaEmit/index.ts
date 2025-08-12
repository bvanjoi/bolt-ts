// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/innerBoundLambdaEmit.ts`, Apache-2.0 License

module M {
  export class Foo {
  }
  var bar = () => { };
}
interface Array<T> {
  toFoo(): M.Foo
}
