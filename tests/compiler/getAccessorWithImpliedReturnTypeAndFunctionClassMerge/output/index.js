// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/getAccessorWithImpliedReturnTypeAndFunctionClassMerge.ts`, Apache-2.0 License

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