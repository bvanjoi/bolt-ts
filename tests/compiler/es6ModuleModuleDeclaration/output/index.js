// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ModuleModuleDeclaration.ts`, Apache-2.0 License
var m1 = {};
(function (m1) {

  var a = 10;
  m1.a = a
  
  var b = 10;
  
  var innerExportedModule = {};
  (function (innerExportedModule) {
  
    var k = 10;
    innerExportedModule.k = k
    
    var l = 10;
    
  })(innerExportedModule);
  m1.innerExportedModule = innerExportedModule;
  
  var innerNonExportedModule = {};
  (function (innerNonExportedModule) {
  
    var x = 10;
    innerNonExportedModule.x = x
    
    var y = 10;
    
  })(innerNonExportedModule);
  m1.innerNonExportedModule = innerNonExportedModule;
  
})(m1);
var m2 = {};
(function (m2) {

  var a = 10;
  m2.a = a
  
  var b = 10;
  
  var innerExportedModule = {};
  (function (innerExportedModule) {
  
    var k = 10;
    innerExportedModule.k = k
    
    var l = 10;
    
  })(innerExportedModule);
  m2.innerExportedModule = innerExportedModule;
  
  var innerNonExportedModule = {};
  (function (innerNonExportedModule) {
  
    var x = 10;
    innerNonExportedModule.x = x
    
    var y = 10;
    
  })(innerNonExportedModule);
  m2.innerNonExportedModule = innerNonExportedModule;
  
})(m2);