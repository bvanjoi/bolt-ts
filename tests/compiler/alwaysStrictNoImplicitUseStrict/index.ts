// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/alwaysStrictNoImplicitUseStrict.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: module=commonjs
//@compiler-options: alwaysStrict
//@compiler-options: noImplicitUseStrict

namespace M {
    export function f() {
        var arguments = [];
        //~^ ERROR: Invalid use of 'arguments' in strict mode.
    }
}