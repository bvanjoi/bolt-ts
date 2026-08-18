// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallInNonStaticMethod.ts`, Apache-2.0 License
class Doing {
  instanceMethod() {}
}
class Other extends Doing {
  instanceMethod() {
    super.instanceMethod();
  }
  lambdaInsideAnInstanceMethod() {
    () => {
      super.instanceMethod();
    };
  }
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