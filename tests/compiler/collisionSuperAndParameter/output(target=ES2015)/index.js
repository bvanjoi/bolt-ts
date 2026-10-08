// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndParameter.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
class Foo {
  a() {
    var lamda = (_super) => (// No Error 
    (x) => (this));
  }// New scope.  So should inject new _this capture
  
  b(_super) {// No Error 
  
    var lambda = () => ((x) => (this));
  }// New scope.  So should inject new _this capture
  
  set c(_super) {// No error
  }
}
class Foo2 extends Foo {
  x() {
    var lamda = (_super) => (// Error 
    (x) => (this));
  }// New scope.  So should inject new _this capture
  
  y(_super) {// Error 
  
    var lambda = () => ((x) => (this));
  }// New scope.  So should inject new _this capture
  
  set z(_super) {// Error
  }
  prop3// no error - no code gen
  ;
  prop4 = {
      doStuff: (_super) => {// should be error
    }    
  };
  constructor(_super) {// should be error
  super();}
}// No error - no code gen
// No error - no code gen
// no error - no code gen
// No error

class Foo4 extends Foo {
  // no code gen - no error
  // no code gen - no error
  constructor(_super) {// should be error
  super();}// no code gen - no error
  // no code gen - no error
  
  y(_super) {// Error 
  
    var lambda = () => ((x) => (this));
  }// New scope.  So should inject new _this capture
  
}