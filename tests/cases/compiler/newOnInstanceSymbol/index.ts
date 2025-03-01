// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/newOnInstanceSymbol.ts`, Apache-2.0 License

class C {}
var x = new C();
new x();
//~^ ERROR: This expression is not constructable.