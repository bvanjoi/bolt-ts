// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/interMixingModulesInterfaces5.ts`, Apache-2.0 License

module A {

  interface B {
      name: string;
      value: number;
  }

  export module B {
      export function createB(): number {
          return null;
      }
  }
}

var x: number = A.B.createB();