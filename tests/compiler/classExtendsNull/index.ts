// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classExtendsNull.ts`, Apache-2.0 License

class C extends null {
    constructor() {
        super();
        //~^ ERROR: A constructor cannot contain a 'super' call when its class extends 'null'.
        return Object.create(null);
    }
}

class D extends null {
    constructor() {
        return Object.create(null);
    }
}