// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reservedNameOnModuleImportWithInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace test {
    interface mi_string { }
    namespace mi_string { }

    // Should error; imports both a type and a module, which means it conflicts with the 'string' type.
    import string = mi_string;
    //~^ ERROR: Import name cannot be 'string'.
}
