// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionRestParameterFunction.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
function f1(_i, ...restParameters) {
  //_i is error
  var _i = 10;
// no error
}
function f1NoError(_i) {
  // no error
  var _i = 10;
// no error
}// no error - no code gen

// no error
function f3(...restParameters) {
  var _i = 10;
// no error
}
function f3NoError() {
  var _i = 10;
// no error
}// no codegen no error

// no codegen no error
function f4(_i, ...rest) {// error
}// no error

// no error
function f4NoError(_i) {// no error
}// no codegen no error
// no codegen no error
// no codegen no error
