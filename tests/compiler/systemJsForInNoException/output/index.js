// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/systemJsForInNoException.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: module=system
//@compiler-options: lib=[es6,dom]
var obj = {
  a: 1  
};
for ( var key in obj) console.log(obj[key]);