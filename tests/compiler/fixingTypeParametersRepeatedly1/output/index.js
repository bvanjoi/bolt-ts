// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fixingTypeParametersRepeatedly1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
f('', (x) => (null), (x) => (x.toLowerCase()));
g('', (x) => (null), (x) => (x.toLowerCase()));