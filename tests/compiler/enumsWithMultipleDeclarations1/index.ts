// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumsWithMultipleDeclarations1.ts`, Apache-2.0 License

enum E {
  A
}

enum E {
  B
  //~^ ERROR: In an enum with multiple declarations, only one declaration can omit an initializer for its first enum element.
}

enum E {
  C
  //~^ ERROR: In an enum with multiple declarations, only one declaration can omit an initializer for its first enum element.
}

let a = E.A;
let b = E.B;
let c = E.C;
let d = E.D;
//~^ ERROR: Property 'D' does not exist on type
