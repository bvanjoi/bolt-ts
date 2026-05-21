// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleUnassignedVariable.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

namespace Bar {
    export var a = 1;
    function fooA() { return a; } // Correct: return Bar.a

    export var b;
    function fooB() { return b; } // Incorrect: return b
}
