// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noUnusedLocals_potentialPredicateUnusedParam.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

function potentialPredicateUnusedParam(a: unknown) {
  //~^ ERROR: 'a' is declared but its value is never read.
  return !!Math.random();
}
