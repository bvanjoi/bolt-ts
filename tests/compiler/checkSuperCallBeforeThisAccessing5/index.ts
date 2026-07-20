// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/checkSuperCallBeforeThisAccessing5.ts`, Apache-2.0 License

class Based { constructor(...arg) { } }
//~^ ERROR: Rest parameter 'arg' implicitly has an 'any[]' type.
class Derived extends Based {
    public x!: number;
    constructor() {
        super(this.x);
        //~^ ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
    }
}
