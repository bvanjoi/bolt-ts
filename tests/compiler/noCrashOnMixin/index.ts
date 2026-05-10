// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noCrashOnMixin.ts`, Apache-2.0 License

//@compiler-options: target=es2015
class Abstract {
    protected constructor() {
    }
}

class Concrete extends Abstract {
}

type Constructor<T = {}> = new (...args: any[]) => T;

function Mixin<TBase extends Constructor>(Base: TBase) {
    return class extends Base {
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