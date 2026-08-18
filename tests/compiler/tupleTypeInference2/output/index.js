// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/tupleTypeInference2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
f([undefined, '']);
f([undefined, '']);
g([[]]);
h([[]]);
h2([[]]);
h2([[]]);