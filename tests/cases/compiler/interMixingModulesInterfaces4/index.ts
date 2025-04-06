// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/interMixingModulesInterfaces4.ts`, Apache-2.0 License

module A {

  export module B {
      export function createB(): number {
          return null;
      }
  }

  interface B {
      name: string;
      value: number;
  }
}

var x : number = A.B.createB();