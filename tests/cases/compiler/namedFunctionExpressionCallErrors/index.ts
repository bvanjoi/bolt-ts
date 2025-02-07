// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/namedFunctionExpressionCallErrors.ts`, Apache-2.0 License

var recurser = function foo() {
};

foo();
//~^ ERROR: Cannot find name 'foo'.

recurser();

(function bar() {
    foo();
    //~^ ERROR: Cannot find name 'foo'.
});

bar();
//~^ ERROR: Cannot find name 'bar'.