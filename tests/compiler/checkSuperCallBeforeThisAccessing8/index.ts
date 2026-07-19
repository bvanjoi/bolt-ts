// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/checkSuperCallBeforeThisAccessing8.ts`, Apache-2.0 License

class Base {
    constructor(...arg) {
    //~^ ERROR: Rest parameter 'arg' implicitly has an 'any[]' type.
    }
}
class Super extends Base {
    constructor() {
        var that = this;
        //~^ ERROR: super' must be called before accessing 'this' in the constructor of a derived class.
        super();
    }
}
