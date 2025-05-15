// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/inheritanceOfGenericConstructorMethod2`, Apache-2.0 License
var M = {};
(function (M) {

  class C1 {}
  M.C1 = C1;
  
  class C2 {}
  M.C2 = C2;
  
})(M);
var N = {};
(function (N) {

  class D1 extends M.C1 {}
  N.D1 = D1;
  
  class D2 extends M.C2 {}
  N.D2 = D2;
  
})(N);
var c = new M.C2();
// no error
var c0 = new M.C2();
// no error
var n = new N.D1();
// no error
var n0 = new N.D1();
var n2 = new N.D2();
// error
var n20 = new N.D2();
// error
var n3 = new N.D2();
// no error, D2<any>
var n30 = new N.D2();