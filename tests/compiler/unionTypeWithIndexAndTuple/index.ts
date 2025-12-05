// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/unionTypeWithIndexAndTuple.ts`, Apache-2.0 License

interface I {
    [index: number]: any;
    someOtherProperty: number;
}
function f(args: ["a"] | I) { }
f(["a"]);

function g(args: ['a']) {}
g(['a']);