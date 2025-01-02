// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/prefixedNumberLiteralAssignToNumberLiteralType.ts`, Apache-2.0 License

let x: 1 = +1;

let y: -1 = -1;

let z: 2 = 1;
//~^ ERROR: Type '1' is not assignable to type '2'.