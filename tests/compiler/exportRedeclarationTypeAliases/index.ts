// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/exportRedeclarationTypeAliases.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export type Foo = number;
export function Foo(): number;
export function Foo(): any {}