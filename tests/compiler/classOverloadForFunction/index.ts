// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classOverloadForFunction.ts`, Apache-2.0 License

class foo { };
//~^ ERROR: Class declaration cannot implement overload list for 'foo'.
function foo() {}
//~^ ERROR: Function with bodies can only merge with classes that are ambient.
