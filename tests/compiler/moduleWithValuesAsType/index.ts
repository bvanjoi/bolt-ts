// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleWithValuesAsType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace A {
    var b = 1;
}

var a: A; // no error
//~^ ERROR: Cannot find name 'A'.