// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classPropertyInferenceFromBroaderTypeConst.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail
var DEFAULT = 'A';
class C {
  D = DEFAULT;
  method() {
    switch (this.D) {
      case 'A':
        break;
      
      case 'B':
        break;// should not error
        
      
    }
  }
}
// D should be AB, not "A"

expectAB(c.D);// ok

c.D = 'B';// ok

// Static property should work the same way
class D {
  static SD = DEFAULT;
}
D.SD = 'B';