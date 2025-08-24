// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumsWithMultipleDeclarations1.ts`, Apache-2.0 License

enum E {
  A
}

enum E {
  B
}

enum E {
  C
}

let a = E.A;
let b = E.B;
let c = E.C;
let d = E.D;
//~^ ERROR: Property 'D' does not exist on type
