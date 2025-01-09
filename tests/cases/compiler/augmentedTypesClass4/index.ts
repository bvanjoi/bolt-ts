// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/augmentedTypesClass4.ts`, Apache-2.0 License

class c3 { public foo() { } } // error
class c3 { public bar() { } } // error
//~^ ERROR: Duplicate identifier 'c3'.