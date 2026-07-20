// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reservedNameOnModuleImport.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace test {
    namespace mstring { }

    // Should be fine; this does not clobber any declared values.
    export import string = mstring;
}

