// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/checkSuperCallBeforeThisAccessing1.ts`, Apache-2.0 License

class Based { }
class Derived extends Based {
    public x: number;
    constructor() {
        super();
        this;
        this.x = 10;
        var that = this;
    }
}