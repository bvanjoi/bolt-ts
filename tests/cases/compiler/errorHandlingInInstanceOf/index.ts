// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/errorHandlingInInstanceOf.ts`, Apache-2.0 License

if (x instanceof String) {
//~^ ERROR: Cannot find name 'x'.
}

var y: any;
if (y instanceof UnknownType) {
//~^ ERROR: Cannot find name 'UnknownType'.
}