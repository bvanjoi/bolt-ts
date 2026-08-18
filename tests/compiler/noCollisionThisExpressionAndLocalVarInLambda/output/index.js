// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noCollisionThisExpressionAndLocalVarInLambda.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var x = {
  doStuff: (callback) => (() => {
    var _this = 2;
    return callback(_this);
  })  
};
alert(x.doStuff((x) => (alert(x))));