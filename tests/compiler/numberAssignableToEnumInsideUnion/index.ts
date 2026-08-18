// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/numberAssignableToEnumInsideUnion.ts`, Apache-2.0 License

//@compiler-options: target=es2015

enum E { A, B }
let n: number;
let z: E | boolean = n;
//~^ ERROR: Variable 'n' is used before being assigned.
