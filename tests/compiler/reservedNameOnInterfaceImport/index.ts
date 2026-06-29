// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reservedNameOnInterfaceImport.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace test {
    interface istring { }

    // Should error; 'test.istring' is a type, so this import conflicts with the 'string' type.
    import string = test.istring;
    //~^ ERROR: Import name cannot be 'string'.
}
