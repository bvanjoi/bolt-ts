// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arguments.ts`, Apache-2.0 License

//@compiler-options: target=es6

function f() {
    var x=arguments[12];
    (() => arguments)();
}

(() => arguments)();
//~^ ERROR: Cannot find name 'arguments'.

interface I {
    method(args: typeof arguments): void;
//~^ ERROR: Cannot find name 'arguments'.
    fn: (args: typeof arguments) => void;
//~^ ERROR: Cannot find name 'arguments'.
    (args: typeof arguments): void;
//~^ ERROR: Cannot find name 'arguments'.
    new (args: typeof arguments): void;
//~^ ERROR: Cannot find name 'arguments'.
    construct: new (args: typeof arguments) => void;
//~^ ERROR: Cannot find name 'arguments'.
}