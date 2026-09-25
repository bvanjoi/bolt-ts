// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedVariablesinNamespaces3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

namespace Validation {
    const lettersRegexp = /^[A-Za-z]+$/;
    const numberRegexp = /^[0-9]+$/;
    //~^ ERROR: 'numberRegexp' is declared but its value is never read.
    export const anotherUnusedVariable = "Dummy value";

    export class LettersOnlyValidator {
        isAcceptable(s2: string) {
            return lettersRegexp.test(s2);
        }
    }
}