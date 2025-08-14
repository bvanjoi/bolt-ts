// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/argumentsBindsToFunctionScopeArgumentList.ts`, Apache-2.0 License

var arguments = 10;
function foo(a) {
    arguments = 10;  /// This shouldnt be of type number and result in error.
    //~^ ERROR: Type 'number' is not assignable to type 'IArguments'.
}