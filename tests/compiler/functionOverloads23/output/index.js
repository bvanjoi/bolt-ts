// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloads23.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo(bar) {
  return 0;
}
foo((a) => {
  var b = a;
});
foo((a) => {
  var b = a;
});