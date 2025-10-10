// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/enumWithPrimitiveName.ts`, Apache-2.0 License

enum string { }
//~^ ERROR: Enum name cannot be 'string'.
enum number { }
//~^ ERROR: Enum name cannot be 'number'.
enum any { }
//~^ ERROR: Enum name cannot be 'any'.
