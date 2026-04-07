// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classOverloadForFunction2.ts`, Apache-2.0 License

function bar(): string;
//~^ ERROR: Function implementation is missing or not immediately following the declaration.
//~| ERROR: Function with bodies can only merge with classes that are ambient.
class bar {}
//~^ ERROR: Class declaration cannot implement overload list for 'bar'.