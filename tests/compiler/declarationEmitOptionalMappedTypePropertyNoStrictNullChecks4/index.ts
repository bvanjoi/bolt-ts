// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitOptionalMappedTypePropertyNoStrictNullChecks4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks=false
//@compiler-options: declaration
//@compiler-options: emitDeclarationOnly

type InputType = { enable_member_receipts?: boolean };
type AsEmptyObject<T> = { [K in keyof T]?: undefined };

export const test = {
  ...({} as AsEmptyObject<InputType>),
};