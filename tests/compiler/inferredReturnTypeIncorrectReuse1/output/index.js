// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferredReturnTypeIncorrectReuse1.ts`, Apache-2.0 License
//@compiler-options: module=commonjs
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: declaration
//@run-fail

var out = t.pipe((s) /** Type<(In: string) => number> */
=> (parseInt(s)));

var out2 = t2.pipe((s) /** Type<(In: string) => number> */
=> (parseInt(s)));