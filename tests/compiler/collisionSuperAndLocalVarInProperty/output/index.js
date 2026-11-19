// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndLocalVarInProperty.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var _super = 10;// No Error

class Foo {
  prop1 = {
      doStuff: () => {
      var _super = 10;// No error
      
    }    
  };
  _super = 10;// No error
  
}
class b extends Foo {
  prop2 = {
      doStuff: () => {
      var _super = 10;// Should be error 
      
    }    
  };
  _super = 10;// No error
  
}