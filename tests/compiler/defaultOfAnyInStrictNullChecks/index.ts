// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultOfAnyInStrictNullChecks.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

function foo() {
    try {
    }
    catch (e) {
        let s = e.message; 
    }
}
