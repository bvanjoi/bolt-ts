// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privacyCheckAnonymousFunctionParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

export var x = 1;  // Makes this an external module
interface Iterator<T> {
}

namespace Query {
    export function fromDoWhile<T>(doWhile: (test: Iterator<T>) => boolean): Iterator<T> {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'Iterator<T>'.
    }

    function fromOrderBy() {
        return fromDoWhile(test => {
            return true;
        });
    }
}