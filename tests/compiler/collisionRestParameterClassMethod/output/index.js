// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionRestParameterClassMethod.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class c1 {
  foo(_i, ...restParameters) {
    //_i is error
    var _i = 10;
  // no error
  }
  fooNoError(_i) {
    // no error
    var _i = 10;
  // no error
  }// no codegen no error
  
  // no codegen no error
  f4(_i, ...rest) {
    // error
    var _i;
  // no error
  }// no error
  
  // no error
  f4NoError(_i) {
    // no error
    var _i;
  // no error
  }
}// No error - no code gen
// no error
// no codegen no error
// no codegen no error
// no error
// no error

class c3 {
  foo(...restParameters) {
    var _i = 10;
  }
  // no error
  fooNoError() {
    var _i = 10;
  }
}