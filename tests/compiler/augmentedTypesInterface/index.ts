// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedTypesInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface i {
    foo(): void;
}

interface i {
    bar(): number;
}

// interface then class
interface i2 {
    foo(): void;
}

class i2 {
    bar() {
        return 1;
    }
}

// interface then enum
interface i3 { // error
    foo(): void;
}
enum i3 { One }; // error
  //~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.

// interface then import
interface i4 {
    foo(): void;
}

//import i4 = require('');  // error