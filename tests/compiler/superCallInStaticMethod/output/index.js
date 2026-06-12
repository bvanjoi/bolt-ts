class Doing {
  static staticMethod() {}
}
class Other extends Doing {
  static staticMethod() {
    super.staticMethod();
  }
  static lambdaInsideAStaticMethod() {
    () => {
      super.staticMethod();
    };
  }
  static objectLiteralInsideAStaticMethod() {
    return {
          a: () => {
        super.staticMethod();
      },
      b: super.staticMethod()      
    }
  }
  static get staticGetter() {
    super.staticMethod();
    return 0
  }
  static set staticGetter(value) {
    super.staticMethod();
  }
  static initializerInAStaticMethod(a = super.staticMethod()) {
    super.staticMethod();
  }
}