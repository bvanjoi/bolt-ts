// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedMultipleParameter1InFunctionExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

var func = function(person: string, person2: string) {
  //~^ ERROR: 'person' is declared but its value is never read.
  //~| ERROR: 'person2' is declared but its value is never read.
    var unused = 20;
  //~^ ERROR: 'unused' is declared but its value is never read.
    person2 = "Dummy value";
}