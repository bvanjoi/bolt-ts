// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndLocalVarInMethod.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var _super = 10; // No Error
class Foo {
    x() {
        var _super = 10; // No error
    }
}
class b extends Foo {
    public foo() {
        var _super = 10; // Should be error 
    }
}
class c extends Foo {
    public foo() {
        var x = () => {
            var _super = 10; // Should be error
        }
    }
}