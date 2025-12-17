// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/collisionSuperAndParameter1.ts`, Apache-2.0 License

class Foo {
}

class Foo2 extends Foo {
    x() {
        var lambda = (_super: number) => { // Error 
        }
    }
}