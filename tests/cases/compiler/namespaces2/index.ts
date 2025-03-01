// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/namespaces2.ts`, Apache-2.0 License

module A {
  export module B {
      export class C { }
  }
}

var c: A.B.C = new A.B.C();