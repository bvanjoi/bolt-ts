// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superAccessCastedCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {
    bar(): void {}
}

class Bar extends Foo {
    x: Number;

    constructor() {
        super();
        this.x = 2;
    }

    bar() {
        super.bar();
        (super.bar as any)();
    }
} 

let b = new Bar();
b.bar()