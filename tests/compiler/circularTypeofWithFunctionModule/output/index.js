// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/circularTypeofWithFunctionModule.ts`, Apache-2.0 License
class Foo {}
function maker(value) {
  return maker.Bar;
}

(function (maker) {

  class Bar extends Foo {}
  maker.Bar = Bar;
  
})(maker);
maker('42');