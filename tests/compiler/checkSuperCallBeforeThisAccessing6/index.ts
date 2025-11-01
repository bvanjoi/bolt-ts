// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/checkSuperCallBeforeThisAccessing6.ts`, Apache-2.0 License

class Base {
    constructor(...arg) {
    }
}
class Super extends Base {
    constructor() {
        (() => this);  // No Error
        super();
    }
}