// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/namedFunctionExpressionCall.ts`, Apache-2.0 License

var recurser = function foo() {
  // using the local name
  foo();

  // using the globally visible name
  recurser();
};


(function bar() {
  bar();
});