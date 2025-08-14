// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/cloduleAcrossModuleDefinitions.ts`, Apache-2.0 License

module A {
  export class B {
      foo() { }
      static bar() { }
  }
}

module A {
  export module B {
      export var x = 1;
  }
}

var b: A.B; // ok
