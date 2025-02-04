// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/noSelfOnVars.ts`, Apache-2.0 License

function foo() {
  function bar() { }
  var x = bar;
}


