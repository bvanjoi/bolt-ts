// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedVariablesinNamespaces1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    const lettersRegexp = /^[A-Za-z]+$/;
    //~^ ERROR: 'lettersRegexp' is declared but its value is never read.
}