class Doing {
  static staticMethod() {}
}
class Other extends Doing {
  // in static method
  static staticMethod() {
    super.staticMethod();
  }
  // in a lambda inside a static method
  static lambdaInsideAStaticMethod() {
    () => {
      super.staticMethod();
    };
  }
  // in an object literal inside a static method
  static objectLiteralInsideAStaticMethod() {
    return {
          a: () => {
        super.staticMethod();
      },
      b: super.staticMethod()      
    };
  }
  static get staticGetter() {
    super.staticMethod();
    return 0;
  }
  static set staticGetter(value) {
    super.staticMethod();
  }
  // in static method
  static initializerInAStaticMethod(a = super.staticMethod()) {
    super.staticMethod();
  }
}