
var MyModule = {};
(function (MyModule) {

  class MyClass {
    get myGetter() {
      var obj = {};
      return obj;
    }
  }
  MyModule.MyClass = MyClass;
  
})(MyModule);