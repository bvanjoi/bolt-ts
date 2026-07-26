// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/baseConstraintOfDecorator.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

export function classExtender<TFunction>(superClass: TFunction, _instanceModifier: (instance: any, args: any[]) => void): TFunction {
    return class decoratorFunc extends superClass {
      //~^ ERROR: Type 'typeof decoratorFunc' is not assignable to type 'TFunction'.
      //~| ERROR: Type 'TFunction' is not a constructor function type.
        constructor(...args: any[]) {
            super(...args);
            _instanceModifier(this, args);
        }
    };
}

class MyClass { private x; }
export function classExtender2<TFunction extends new (...args: string[]) => MyClass>(superClass: TFunction, _instanceModifier: (instance: any, args: any[]) => void): TFunction {
    return class decoratorFunc extends superClass {
      //~^ ERROR: A mixin class must have a constructor with a single rest parameter of type 'any[]'.
        constructor(...args: any[]) {
            super(...args);
            _instanceModifier(this, args);
        }
    };
}
