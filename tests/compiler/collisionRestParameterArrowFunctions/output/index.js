// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionRestParameterArrowFunctions.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var f1 = (_i, ...restParameters) => {
  var _i = 10;
};
var f1NoError = (_i) => {
  var _i = 10;
};
var f2 = (...restParameters) => {
  var _i = 10;
};
var f2NoError = () => {
  var _i = 10;
};