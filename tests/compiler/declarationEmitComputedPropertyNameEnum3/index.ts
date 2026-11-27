// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitComputedPropertyNameEnum3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
//@compiler-options: emitDeclarationOnly

import { type Type } from "./type";

export const foo = { ...({} as Type) };
