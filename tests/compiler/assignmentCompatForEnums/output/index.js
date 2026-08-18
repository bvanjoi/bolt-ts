// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignmentCompatForEnums.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var TokenType = {};
(function (TokenType) {

  TokenType[TokenType['One'] = 0] = 'One'
  TokenType[TokenType['Two'] = 0] = 'Two'
})(TokenType);
;
var list = {};
function returnType() {
  return null;
}
function foo() {
  var x = returnType();
  var x = list['one'];
}