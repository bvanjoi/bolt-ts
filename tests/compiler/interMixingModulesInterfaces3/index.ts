// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/interMixingModulesInterfaces3.ts`, Apache-2.0 License

module A {

  module B {
      export function createB(): B {
          return null;
      }
  }

  export interface B {
      name: string;
      value: number;
  }
}

var x: A.B = null;