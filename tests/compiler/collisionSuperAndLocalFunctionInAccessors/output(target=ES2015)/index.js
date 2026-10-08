// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndLocalFunctionInAccessors.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
function _super() {// No error
}
class Foo {
  get prop1() {
    function _super() {// No error
    }
    return 10;
  }
  set prop1(val) {
    function _super() {// No error
    }
  }
}
class b extends Foo {
  get prop2() {
    function _super() {// Should be error
    }
    return 10;
  }
  set prop2(val) {
    function _super() {// Should be error
    }
  }
}
class c extends Foo {
  get prop2() {
    var x = () => {
      function _super() {// Should be error
      }
    };
    return 10;
  }
  set prop2(val) {
    var x = () => {
      function _super() {// Should be error
      }
    };
  }
}