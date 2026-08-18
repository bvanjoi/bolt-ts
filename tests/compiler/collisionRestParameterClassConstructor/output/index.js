// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionRestParameterClassConstructor.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class c1 {
  constructor(_i, ...restParameters) {var _i = 10;}
}
class c1NoError {
  constructor(_i) {var _i = 10;}
}
class c2 {
  constructor(...restParameters) {var _i = 10;}
}
class c2NoError {
  constructor() {var _i = 10;}
}
class c3 {
  constructor(_i, ...restParameters) {
    var _i = 10;
    this._i = _i
    
    }
}
class c3NoError {
  constructor(_i) {
    var _i = 10;
    this._i = _i
    }
}
class c5 {
  constructor(_i, ...rest) {var _i;}
}
class c5NoError {
  constructor(_i) {var _i;}
}