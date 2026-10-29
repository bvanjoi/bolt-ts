// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumMemberNameNonIdentifier.ts`, Apache-2.0 License
//@compiler-options: declaration
var E = {};
(function (E) {

  E[E['regular'] = 0] = // Greek Capital Yot (U+037F) - valid identifier in ES2015+ but NOT in ES5
  'regular'
  E[E['hyphen-member'] = 1] = 'hyphen-member'
  E[E['123startsWithNumber'] = 2] = '123startsWithNumber'
  E[E['has space'] = 3] = 'has space'
  E[E['Ϳ'] = 4] = 'Ϳ'
})(E);
var a = E['hyphen-member'];
var b = E['123startsWithNumber'];
var c = E['has space'];
var d = E.regular;
var e = E.Ϳ;