// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noUnusedLocals_selfReference_skipsBlockLocations.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noUnusedLocals

namespace n {
    function f() {
      //~^ ERROR: 'f' is declared but its value is never read.
        f;
    }

    switch (0) {
        case 0:
            function g() {
      //~^ ERROR: 'g' is declared but its value is never read.
                g;
            }
        default:
            function h() {
      //~^ ERROR: 'h' is declared but its value is never read.
                h;
            }
    }
}
