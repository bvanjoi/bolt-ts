// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/mismatchedClassConstructorVariable.ts`, Apache-2.0 License

var baz: foo;
class baz { }
//~^ ERROR: Duplicate identifier 'baz'.
class foo { }