// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/augmentedTypesClass2.ts`, Apache-2.0 License


// Checking class with other things in type space not value space

// class then interface
class c11 {
    foo() {
        return 1;
    }
}

interface c11 {
    bar(): void;
}

// class then class - covered
// class then enum 
class c33 {
    foo() {
        return 1;
    }
}
enum c33 { One };
//~^ ERROR: Enum declarations can only merge with namespace or other enum declarations.

// class then import
class c44 {
    foo() {
        return 1;
    }
}

