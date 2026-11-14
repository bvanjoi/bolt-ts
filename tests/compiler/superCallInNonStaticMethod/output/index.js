// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallInNonStaticMethod.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Doing {
  instanceMethod() {}
}
class Other extends Doing {
  // in instance method
  instanceMethod() {
    super.instanceMethod();
  }
  lambdaInsideAnInstanceMethod// in a lambda inside a instance method
  () {
    () => {
      super.instanceMethod();
    };
  }
  objectLiteralInsideAnInstanceMethod(// in an object literal inside a instance method
  ) {
    return {
          a: () => {
        super.instanceMethod();
      },
      b: super.instanceMethod()      
    };
  }
  get accessor(// in a getter
  ) {
    super.instanceMethod();
    return 0;
  }
  set accessor(value// in a setter
  ) {
    super.instanceMethod();
  }
  constructor() {super();super.instanceMethod();}
  propertyInitializer = super.instanceMethod();
  functionProperty = () => {
    super.instanceMethod();
  };
}