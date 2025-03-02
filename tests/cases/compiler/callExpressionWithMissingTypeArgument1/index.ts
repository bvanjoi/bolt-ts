// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/callExpressionWithMissingTypeArgument1.ts`, Apache-2.0 License

Foo<a,,b>();
//~^ ERROR: Identifier expected.
//~| ERROR: Cannot find name 'Foo'.
//~| ERROR: Cannot find name 'a'.
//~| ERROR: Cannot find name 'b'.
