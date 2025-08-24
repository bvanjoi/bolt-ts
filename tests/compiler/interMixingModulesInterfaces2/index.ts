// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/interMixingModulesInterfaces2.ts`, Apache-2.0 License

module A {

  export interface B {
      name: string;
      value: number;
  }

  module B {
      export function createB(): B {
          return null;
      }
  }
}

var x: A.B = null;