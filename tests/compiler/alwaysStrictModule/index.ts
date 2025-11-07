// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/alwaysStrictModule.ts`, Apache-2.0 License

//@compiler-options: module=commonjs
//@compiler-options: alwaysStrict

namespace M {
    export function f() {
        var arguments = [];
        //~^ ERROR: Invalid use of 'arguments' in strict mode.
    }
}
