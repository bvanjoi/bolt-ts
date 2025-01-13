// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/unaryOperators1.ts`, Apache-2.0 License

+foo;
//~^ ERROR: Cannot find name 'foo'.
-bar;
//~^ ERROR: Cannot find name 'bar'.
~quux;
//~^ ERROR: Cannot find name 'quux'.