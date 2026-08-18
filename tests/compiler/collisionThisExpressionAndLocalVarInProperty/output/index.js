// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndLocalVarInProperty.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class class1 {
  prop1 = {
      doStuff: (callback) => (() => {
      var _this = 2;
      return callback(this);
    })    
  };
}
class class2 {
  constructor() {var _this = 2;}
  prop1 = {
      doStuff: (callback) => (() => (callback(this)))    
  };
}