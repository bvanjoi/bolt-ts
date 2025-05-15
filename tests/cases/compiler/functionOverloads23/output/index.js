// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads23.ts`, Apache-2.0 License


function foo(bar) {
  return 0
}
foo((a) => {
  var b = a;
});
foo((a) => {
  var b = a;
});