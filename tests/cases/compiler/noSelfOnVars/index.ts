// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/noSelfOnVars.ts`, Apache-2.0 License

function foo() {
  function bar() { }
  var x = bar;
}


