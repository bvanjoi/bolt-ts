// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constraintsUsedInPrototypeProperty.ts`, Apache-2.0 License

class Foo<T extends number, U, V extends string> { }
Foo.prototype; // Foo<any, any, any>