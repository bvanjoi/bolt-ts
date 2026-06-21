// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleVariableArrayIndexer.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Bar {
    export var a = 1;
    var t = undefined[a][a]; // CG: var t = undefined[Bar.a][a];
    //~^ ERROR: The value 'undefined' cannot be used here.
}
