// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionSuperAndNameResolution.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: lib=[es5]

var console: {
    log(message: any);
}
var _super = 10; // No error
class base {
}
class Foo extends base {
    x() {
        console.log(_super); // Error as this doesnt not resolve to user defined _super
    }
}
