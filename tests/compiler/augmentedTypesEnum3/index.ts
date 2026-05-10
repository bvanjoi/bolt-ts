// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/augmentedTypesEnum3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace E {
    var t;
}
enum E { }

enum F { }
namespace F { var t; }

namespace A {
    var o;
}
enum A {
    b
}
enum A {
    c //~ERROR: In an enum with multiple declarations, only one declaration can omit an initializer for its first enum element.
}
namespace A {
    var p;
}