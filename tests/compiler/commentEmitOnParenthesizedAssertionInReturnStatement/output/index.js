// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentEmitOnParenthesizedAssertionInReturnStatement.ts`, Apache-2.0 License
//@compiler-options: strict
//@compiler-options: lib=[esnext]
//@compiler-options: target=esnext
//@compiler-options: removeComments=false
export class Foo {
  client = {
      getThing: () => (Promise.resolve(''))    
  };
  foo() {
    return (/* TODO: Avoid using type assertions, please refactor. */ this.client.getThing());
  }
}