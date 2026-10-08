// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/emptyThenWithoutWarning.ts`, Apache-2.0 License
var a = 4;
if (a === 1 || a === 2 || a === 3) {} else {
  var message = 'Ooops';
}
