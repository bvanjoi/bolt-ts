// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/callOnClass.ts`, Apache-2.0 License

class C { }
var c = C();
//~^ ERROR: Value of type 'typeof C' is not callable.