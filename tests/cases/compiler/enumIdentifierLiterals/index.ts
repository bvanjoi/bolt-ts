// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/enumIdentifierLiterals.ts`, Apache-2.0 License

enum Nums {
    1.0,            //~ ERROR: An enum member cannot have a numeric name.
    11e-1,          //~ ERROR: An enum member cannot have a numeric name.
    0.12e1,         //~ ERROR: An enum member cannot have a numeric name.
    "13e-1",
    0xF00D          //~ ERROR: An enum member cannot have a numeric name.
}