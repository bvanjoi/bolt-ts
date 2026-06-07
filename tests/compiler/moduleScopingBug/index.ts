// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleScopingBug.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace M

{

    var outer: number;

    function f() {

        var inner = outer;   // Ok

    }

    class C {

        constructor() {
            var inner = outer;   // Ok
        }

    }

    namespace X {

        var inner = outer;   // Error: outer not visible

    }

}

