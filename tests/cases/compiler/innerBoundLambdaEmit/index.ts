// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/innerBoundLambdaEmit.ts`, Apache-2.0 License

module M {
  export class Foo {
  }
  var bar = () => { };
}
interface Array<T> {
  toFoo(): M.Foo
}
