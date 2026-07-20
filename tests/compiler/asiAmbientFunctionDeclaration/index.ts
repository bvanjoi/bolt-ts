// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/asiAmbientFunctionDeclaration.ts`, Apache-2.0 License

declare function foo()
//~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
