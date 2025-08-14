// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/missingArgument1.ts`, Apache-2.0 License

foo(a,,b);
//~^ ERROR: Argument expression expected.
//~| ERROR: Cannot find name 'foo'.
//~| ERROR: Cannot find name 'a'.
//~| ERROR: Cannot find name 'b'.
