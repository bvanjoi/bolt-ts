// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noSubtypeReduction.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: strictNullChecks
var Meat = {};
(function (Meat) {

  Meat[Meat['Sausage'] = 0] = 'Sausage'
  Meat[Meat['Bacon'] = 0] = 'Bacon'
})(Meat);
var sausage = Meat.Sausage;
var valueSausage = Meat[sausage];
var bacon = Meat.Bacon;
var valueBacon = Meat[bacon];
var union = Meat.Bacon;
var valueUnion = Meat[union];
var value = Meat[0];
var valueUndefined = 'testing';
var value2 = Meat[valueUndefined];
var A = {};
(function (A) {

  A[A['a'] = 0] = 'a'
  A[A['b'] = 0] = 'b'
  A[A['c'] = 0] = 'c'
})(A);
var B = {};
(function (B) {

  B[B['x'] = 0] = 'x'
  B[B['y'] = 0] = 'y'
  B[B['z'] = 0] = 'z'
})(B);
var value3 = A[B.x];