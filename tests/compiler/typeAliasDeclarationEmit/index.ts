// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeAliasDeclarationEmit.ts`, Apache-2.0 License

//@compiler-options: target=es2015

export type callback<T> = () => T;

export type CallbackArray<T extends callback> = () => T;
//~^ ERROR: Generic type 'callback' requires 1 type argument.