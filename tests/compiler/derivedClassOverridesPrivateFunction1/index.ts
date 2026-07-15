// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/derivedClassOverridesPrivateFunction1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class BaseClass {
    constructor() {
        this._init();
    }
    private _init() {
    }
}
class DerivedClass extends BaseClass {
  //~^ ERROR: Class 'DerivedClass' incorrectly extends base class 'BaseClass'.
    constructor() {
        super();
    }
    private _init() {
    }
}
new DerivedClass();