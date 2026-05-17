// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndLocalFunctionInConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function _super() { // No error
}
class Foo {
    constructor() {
        function _super() { // No error
        }
    }
}
class b extends Foo {
    constructor() {
        super();
        function _super() { // Should be error
        }
    }
}
class c extends Foo {
    constructor() {
        super();
        var x = () => {
            function _super() { // Should be error
            }
        }
    }
}