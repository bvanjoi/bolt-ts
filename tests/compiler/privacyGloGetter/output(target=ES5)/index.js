// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privacyGloGetter.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
var m1 = {};
(function (m1) {

  class C1_public {
    f1() {}
  }
  m1.C1_public = C1_public;
  
  class C2_private {}
  
  class C3_public {
    get p1_private() {
      return new C1_public();
    }
    set p1_private(m1_c3_p1_arg) {}
    get p2_private() {
      return new C1_public();
    }
    set p2_private(m1_c3_p2_arg) {}
    get p3_private() {
      return new C2_private();
    }
    set p3_private(m1_c3_p3_arg) {}
    get p4_public() {// error
    
      return new C2_private();//error
      
    }
    set p4_public(m1_c3_p4_arg) {// error
    }
  }
  m1.C3_public = C3_public;
  
  class C4_private {
    get p1_private() {
      return new C1_public();
    }
    set p1_private(m1_c3_p1_arg) {}
    get p2_private() {
      return new C1_public();
    }
    set p2_private(m1_c3_p2_arg) {}
    get p3_private() {
      return new C2_private();
    }
    set p3_private(m1_c3_p3_arg) {}
    get p4_public() {
      return new C2_private();
    }
    set p4_public(m1_c3_p4_arg) {}
  }
  
})(m1);
class C6_public {}
class C7_public {
  get p1_private() {
    return new C6_public();
  }
  set p1_private(m1_c3_p1_arg) {}
  get p2_private() {
    return new C6_public();
  }
  set p2_private(m1_c3_p2_arg) {}
}