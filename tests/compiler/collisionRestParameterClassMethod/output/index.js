// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionRestParameterClassMethod.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class c1 {
  foo(_i, ...restParameters) {
    var _i = 10;
  }
  fooNoError(_i) {
    var _i = 10;
  }
  f4(_i, ...rest) {
    var _i;
  }
  f4NoError(_i) {
    var _i;
  }
}
class c3 {
  foo(...restParameters) {
    var _i = 10;
  }
  fooNoError() {
    var _i = 10;
  }
}