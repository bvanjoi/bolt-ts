// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/ambientEnum1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

    declare enum E1 {
        y = 4.23
    }
    
    // Ambient enum with computer member
    declare enum E2 {
        x = 'foo'.length //~ERROR: In ambient enum declarations member initializer must be constant expression.
    }