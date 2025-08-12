// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/namespaces1.ts`, Apache-2.0 License

module X {
  export module Y {
      export interface Z { }
  }
  export interface Y { }
}

var x: X.Y.Z;
var x2: X.Y;