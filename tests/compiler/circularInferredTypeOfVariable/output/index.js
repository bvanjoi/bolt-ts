// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/circularInferredTypeOfVariable.ts`, Apache-2.0 License
(async () => {
  function foo(p) {
    return [];
  }
  function bar(p) {
    return [];
  }
  var a1 = [];
  while (true) {
    var a2 = foo(a1);
    a1 = await bar(a2);
  }
});