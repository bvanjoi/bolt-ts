// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classPropertyInferenceFromBroaderTypeConst.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
var DEFAULT = 'A';
class C {
  D = DEFAULT;
  method() {
    switch (this.D) {
      case 'A':
        break;
      
      case 'B':
        break;
      
    }
  }
}

expectAB(c.D);
c.D = 'B';
class D {
  static SD = DEFAULT;
}
D.SD = 'B';