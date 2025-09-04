// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/augmentedTypesClass.ts`, Apache-2.0 License

//// class then var
class c1 { public foo() { } }
var c1 = 1; // error
//~^ ERROR: Duplicate identifier 'c1'.
//~| ERROR: Type 'number' is not assignable to type 'typeof c1'.

//// class then enum
class c4 { public foo() { } }
enum c4 { One } // error
//~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.