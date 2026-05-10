// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedTypesEnum2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// enum then interface
enum e1 { One } // error

interface e1 { // error
  //~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.
    foo(): void;
}

// interface then enum works

// enum then class
enum e2 { One }; // error
class e2 { // error
  //~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.
    foo() {
        return 1;
    }
}

//enum then enum - covered
//enum then import - covered