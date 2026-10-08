// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privacyCheckTypeOfInvisibleModuleNoError.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
var Outer = {};
(function (Outer) {

  var Inner = {};
  (function (Inner// Since we dont unwind inner any more, it is error here
  ) {
  
    var m;
    Inner.m = m
    
  })(Inner);
  
  var f;
  Outer.f = f
  
})(Outer);