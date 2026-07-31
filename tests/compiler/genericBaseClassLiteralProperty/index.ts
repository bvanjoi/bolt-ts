// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericBaseClassLiteralProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class BaseClass<T> {
    public _getValue1: { (): T; };
    //~^ ERROR: Property '_getValue1' has no initializer and is not definitely assigned in the constructor.
    public _getValue2: () => T;
    //~^ ERROR: Property '_getValue2' has no initializer and is not definitely assigned in the constructor.
}

class SubClass extends BaseClass<number> {
    public Error(): void {

        var x : number = this._getValue1();
        var y : number = this._getValue2();
    }
}