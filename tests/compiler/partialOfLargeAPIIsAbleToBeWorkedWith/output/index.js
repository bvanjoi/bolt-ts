// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/partialOfLargeAPIIsAbleToBeWorkedWith.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
var obj = {};

for ( var k of keys) {
  obj[k] = () => ('12');
}
var obj2 = {};
for ( var k of keys) {
  obj2[k] = () => ('12');
}