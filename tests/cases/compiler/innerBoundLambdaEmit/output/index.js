// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/innerBoundLambdaEmit.ts`, Apache-2.0 License
var M = {};
(function (M) {

  class Foo {}
  M.Foo = Foo;
  
  var bar = () => {};
  
})(M);
