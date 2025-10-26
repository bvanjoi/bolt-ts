// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/checkSuperCallBeforeThisAccessing7.ts`, Apache-2.0 License

class Base {
    constructor(func: ()=>Base) {
    }
}
class Super extends Base {
    constructor() {
        super((() => this)); // No error
    }
}