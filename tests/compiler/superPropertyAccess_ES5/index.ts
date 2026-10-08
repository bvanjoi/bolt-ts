// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superPropertyAccess_ES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

class MyBase {
  getValue(): number { return 1; }
  get value(): number { return 1; }
}

class MyDerived extends MyBase {
  constructor() {
    super();

    const f1 = super.getValue();
    const f2 = super.value;
    //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
  }
}

var d = new MyDerived();
var f3 = d.value;

class A {
    private _property: string;
    //~^ ERROR: Property '_property' has no initializer and is not definitely assigned in the constructor.
    get property() { return this._property; }
    set property(value: string) { this._property = value }
}

class B extends A {
    set property(value: string) {
        super.property = value + " addition";
    //~[target=ES5]^ ERROR: Only public and protected methods of the base class are accessible via the 'super' keyword.
    }
}