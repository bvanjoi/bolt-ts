// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndParameter.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: lib=[es5]
class Foo {
  x() {
    var _this = 10;
    // Local var. No this capture in x(), so no conflict.
    function inner(_this) {
      // Error 
      return (x) => (this);
    // New scope.  So should inject new _this capture into function inner
    }
  }
  y() {
    var lamda = (_this) => ((// Error 
    x) => (this));
  // New scope.  So should inject new _this capture
  }
  z(_this) {
    // Error 
    var lambda = () => ((x) => (this));
  // New scope.  So should inject new _this capture
  }
  x1() {
    var _this = 10;
    function // Local var. No this capture in x(), so no conflict.
    inner(_this) {// No Error 
    }
  }
  y1() {
    var lamda = (_this) => {// No Error 
    };
  }
  z1(_this) {
    // No Error 
    var lambda = () => {};
  }
}
class Foo1 {
  constructor(_this) {// Error
    var x2 = {
          doStuff: (callback) => (() => (callback(this)))      
    };}
}

function f1(_this) {
  (x) => {
    console.log(this.x);
  };
}// no error - no code gen
// no error - no code gen

// no error
class Foo3 {
  // no code gen - no error
  // no code gen - no error
  constructor(_this) {// Error
    var x2 = {
          doStuff: (callback) => (() => (callback(this)))      
    };}// no code gen - no error
  
  // no code gen - no error
  z(_this) {
    // Error 
    var lambda = () => ((x) => (this));
  // New scope.  So should inject new _this capture
  }
}
// no code gen - no error

// no code gen - no error
function f3(_this) {
  (x) => {
    console.log(this.x);
  };
}// no code gen - no error
// no code gen - no error
// no code gen - no error
// no code gen - no error
// no code gen - no error
