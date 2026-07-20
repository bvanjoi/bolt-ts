// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spreadObjectNoCircular1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Box = {
  content?: Foo | Box;
};

declare const b: Box;

class Foo {
  get foo() {
    return {
      content: this as Foo | Box,
      ...b,
    };
  }
}