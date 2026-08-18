// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndLocalFunctionInProperty.ts`, Apache-2.0 License
function _super() {}
class Foo {
  prop1 = {
      doStuff: () => {
      function _super() {}
    }    
  };
}
class b extends Foo {
  prop2 = {
      doStuff: () => {
      function _super() {}
    }    
  };
}