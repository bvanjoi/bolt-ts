// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noCrashOnMixin2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/62921

class Abstract {
    protected constructor() {
    }
}

class Concrete extends Abstract {
}

type Constructor<T = {}> = new (...args?: any[]) => T;
//~^ ERROR: A rest parameter must be of an array type.
//~| ERROR: A rest parameter cannot be optional.

function Mixin<TBase extends Constructor>(Base: TBase) {
    return class extends Base {
  //~^ ERROR: A mixin class must have a constructor with a single rest parameter of type 'any[]'.
    };
}

class Empty {
}

class CrashTrigger extends Mixin(Empty) {
    public trigger() {
        new Concrete();
        //~^ ERROR: Constructor of class 'Abstract' is protected and only accessible within the class declaration.
    }
}
