// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/intrinsics.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: declaration

var hasOwnProperty: hasOwnProperty; // Error
//~^ ERROR: Cannot find name 'hasOwnProperty'.

namespace m1 {
    export var __proto__;
    interface __proto__ {}

    class C<T extends { __proto__: __proto__ }> { }
}

__proto__ = 0; // Error, __proto__ not defined
//~^ ERROR: Cannot find name '__proto__'.
m1.__proto__ = 0;

class Foo<__proto__> { }
var foo: (__proto__: number) => void;