// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/cloduleAndTypeParameters.ts`, Apache-2.0 License

class Foo<T extends Foo.Bar> {
  constructor() {
  }
}

module Foo {
  export interface Bar {
    bar(): void;
  }

  export class Baz {
  }
}
