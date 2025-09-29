// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/throwWithoutNewLine2.ts`, Apache-2.0 License

throw
//~^ ERROR: Line break not permitted here.
a;
//~^ ERROR: Cannot find name 'a'.