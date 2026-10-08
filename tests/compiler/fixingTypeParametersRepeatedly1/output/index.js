// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fixingTypeParametersRepeatedly1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail
f('', (x) => (null), (x) => (x.toLowerCase()));// First overload of g should type check just like f

g('', (x) => (null), (x) => (x.toLowerCase()));