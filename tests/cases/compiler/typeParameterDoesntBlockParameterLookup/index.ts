// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeParameterDoesntBlockParameterLookup.ts`, Apache-2.0 License

declare function f<Foo extends Bar, Bar>(Bar: any): void