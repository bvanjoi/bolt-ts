// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceWithCommaSeparators.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var v: { bar(): void, baz }
interface Foo { bar(): void, baz }
