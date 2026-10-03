// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndLocalVarInAccessors.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

var _super = 10; // No Error
class Foo {
    get prop1(): number {
        var _super = 10; // No error
        return 10;
    }
    set prop1(val: number) {
        var _super = 10; // No error
    }
}
class b extends Foo {
    get prop2(): number {
        var _super = 10; // Should be error
        return 10;
    }
    set prop2(val: number) {
        var _super = 10; // Should be error
    }
}
class c extends Foo {
    get prop2(): number {
        var x = () => {
            var _super = 10; // Should be error
        }
        return 10;
    }
    set prop2(val: number) {
        var x = () => {
            var _super = 10; // Should be error
        }
    }
}