// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/resolveTypeAliasWithSameLetDeclarationName1.ts`, Apache-2.0 License

class C { }
type baz = C;
let baz: baz;
