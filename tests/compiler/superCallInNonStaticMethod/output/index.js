class Doing {
  instanceMethod() {}
}
class Other extends Doing {
  // in instance method
  instanceMethod() {
    super.instanceMethod();
  }
  // in a lambda inside a instance method
  lambdaInsideAnInstanceMethod() {
    () => {
      super.instanceMethod();
    };
  }
  // in an object literal inside a instance method
  objectLiteralInsideAnInstanceMethod() {
    return {
          a: () => {
        super.instanceMethod();
      },
      b: super.instanceMethod()      
    };
  }
  get accessor() {
    super.instanceMethod();
    return 0;
  }
  set accessor(value) {
    super.instanceMethod();
  }
  constructor() {super();super.instanceMethod();}
  propertyInitializer = super.instanceMethod();
  functionProperty = () => {
    super.instanceMethod();
  };
}