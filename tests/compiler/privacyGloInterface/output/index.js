// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privacyGloInterface.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var m1 = {};
(function (m1) {

  class C1_public {
    f1() {}
  }
  m1.C1_public = C1_public;
  
  class C2_private {}
  
})(m1);
class C5_public {
  f1() {}
}
