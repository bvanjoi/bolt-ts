// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/spreadBooleanRespectsFreshness.ts`, Apache-2.0 License
//@compiler-options: target=es2015


foo1 = [...Array.isArray(foo2) ? foo2 : [foo2]];