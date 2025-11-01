// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/checkSuperCallBeforeThisAccessing2.ts`, Apache-2.0 License

class Based { }
class Derived extends Based {
    public x: number;
    constructor() {
        this.x = 100;
        //~^ ERROR: 'super' must be called before accessing 'this' in the constructor of a derived class.
        super();
        this.x = 10;
        var that = this;
    }
}