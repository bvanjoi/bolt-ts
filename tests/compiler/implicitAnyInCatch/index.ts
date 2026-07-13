// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/implicitAnyInCatch.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny
//@compiler-options: useUnknownInCatchVariables

// this should not be an error
try { } catch (error) {
    if (error.number === -2147024809) { }
}
for (var key in this) { }

class C {
    public temp() {
        for (var x in this) {
        }
    }
}


