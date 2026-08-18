// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/contextualTypeIterableUnions.ts`, Apache-2.0 License
//@compiler-options: target=esnext
//@compiler-options: strict
new DMap([['1', 2]]);
var i1 = [{
  a: true  
}];
var i2 = [{
  b: false  
}];
var i3 = [2];