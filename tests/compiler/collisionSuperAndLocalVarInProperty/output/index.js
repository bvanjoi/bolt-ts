// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndLocalVarInProperty.ts`, Apache-2.0 License
var _super = 10;
class Foo {
  prop1 = {
      doStuff: () => {
      var _super = 10;
    }    
  };
  _super = 10;
}
class b extends Foo {
  prop2 = {
      doStuff: () => {
      var _super = 10;
    }    
  };
  _super = 10;
}