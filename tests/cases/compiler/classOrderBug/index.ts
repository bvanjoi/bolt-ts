// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/classOrderBug.ts`, Apache-2.0 License

class bar {
    public baz: foo;
    constructor() {

        this.baz = new foo();

    }

}

class baz {}
class foo extends baz {}


