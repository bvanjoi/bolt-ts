// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/objectLitStructuralTypeMismatch.ts`, Apache-2.0 License

var x: { a: number; } = { b: 5 };
//~^ ERROR: Object literal may only specify known properties, and 'b' does not exist.