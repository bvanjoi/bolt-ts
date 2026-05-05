
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitTypeAliasTypeParameterExtendingUnknownSymbol.ts`, Apache-2.0 License

//@compiler-options: target=es6
//@compiler-options: declaration

type A<T extends Unknown> = {}
//~^ ERROR: Cannot find name 'Unknown'.