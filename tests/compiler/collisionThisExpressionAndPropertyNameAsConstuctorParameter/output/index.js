// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionThisExpressionAndPropertyNameAsConstuctorParameter.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class Foo2 {
  constructor(_this) {//Error
  var lambda = () => ((x) => (this));}// New scope.  So should inject new _this capture
  
}
class Foo3 {
  constructor(_this) {// Error
  var lambda = () => ((x) => (this));}// New scope.  So should inject new _this capture
  
}
class Foo4 {
  // No code gen - no error
  // No code gen - no error
  constructor(_this) {// Error
  var lambda = () => ((x) => (this));}// New scope.  So should inject new _this capture
  
}
class Foo5 {
  // No code gen - no error
  // No code gen - no error
  constructor(_this) {// Error
  var lambda = () => ((x) => (this));}// New scope.  So should inject new _this capture
  
}