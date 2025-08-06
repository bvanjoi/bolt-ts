// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/checkSuperCallBeforeThisAccessing4.ts`, Apache-2.0 License

class Based { }
class Derived extends Based {
    public x: number;
    constructor() {
        (() => {
            this;  // No error
        });
        () => {
            this;  // No error
        };
        (() => {
            this;  // No error
        })();
        super();
        super();
        this.x = 10;
        var that = this;
    }
}