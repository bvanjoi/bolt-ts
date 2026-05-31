// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralsAgainstUnionsOfArrays01.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var _super = 10; // No Error
class Foo {
    constructor() {
        var _super = 10; // No error
    }
}
class b extends Foo {
    constructor() {
        super();
        var _super = 10; // Should be error 
    }
}
class c extends Foo {
    constructor() {
        super();
        var x = () => {
            var _super = 10; // Should be error
        }
    }
}