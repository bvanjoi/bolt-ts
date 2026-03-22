// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/argumentsBindsToFunctionScopeArgumentList.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: alwaysStrict

var arguments = 10;
//~^ ERROR: Invalid use of 'arguments' in strict mode.
function foo(a) {
    arguments = 10;  /// This shouldnt be of type number and result in error.
    //~^ ERROR: Type 'number' is not assignable to type 'IArguments'.
    //~| ERROR: Invalid use of 'arguments' in strict mode.
}

function f() {
    const a = new Map<string, number>();
    [...a.entries()];
}