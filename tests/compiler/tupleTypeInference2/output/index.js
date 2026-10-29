// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/tupleTypeInference2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
// Repro from #22564
f([undefined, '']);// T: never

f([undefined, '']);// T: void
// Repro from #22563

g([[]]);// U: {}

h([[]]);// U: {}
// Repro from #22562

h2([[]]);// T: never

h2([[]]);