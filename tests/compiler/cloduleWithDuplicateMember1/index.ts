// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/cloduleWithDuplicateMember1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    get x() { return 1; }
    static get x() {
        return '';
    }
    static foo() { }
}

namespace C {
    export var x = 1;
      //~^ ERROR: Duplicate identifier 'x'.
}
namespace C {
    export function foo() { }
    //~^ ERROR: Duplicate identifier 'foo'.
    export function x() { }
    //~^ ERROR: Duplicate identifier 'x'.
}