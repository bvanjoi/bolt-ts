// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nongenericConditionalNotPartiallyComputed.ts`, Apache-2.0 License

//@compiler-options: target=es2015
type A = Array<number> extends Array<any> ? Array<number> extends Array<infer T> ? T : never : never

const a: A = '42';
//~^ ERROR: Type 'string' is not assignable to type 'number'.