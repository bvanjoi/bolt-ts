// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superCallInStaticMethod.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Doing {
  static staticMethod() {}
}
class Other extends Doing {
  // in static method
  static staticMethod() {
    super.staticMethod();
  }
  static // in a lambda inside a static method
  lambdaInsideAStaticMethod() {
    () => {
      super.staticMethod();
    };
  }
  static objectLiteralInsideAStaticMethod// in an object literal inside a static method
  () {
    return {
          a: () => {
        super.staticMethod();
      },
      b: super.staticMethod()      
    };
  }
  static get staticGetter// in a getter
  () {
    super.staticMethod();
    return 0;
  }
  static set staticGetter(// in a setter
  value) {
    super.staticMethod();
  }
  // in static method
  static initializerInAStaticMethod(a = super.staticMethod()) {
    super.staticMethod();
  }
}