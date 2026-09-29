// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileTypeAnnotationVisibilityErrorReturnTypeOfFunction.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

namespace m {
    class private1 {
    }

    export class public1 {
    }

    // Directly using names from this module
    function foo1(): private1 {
        return;
        //~^ ERROR: Type 'undefined' is not assignable to type 'private1'.
    }
    function foo2() {
        return new private1();
    }

    export function foo3(): private1 {
        return;
        //~^ ERROR: Type 'undefined' is not assignable to type 'private1'.
    }
    export function foo4() {
        return new private1();
    }

    function foo11(): public1 {
        return;
        //~^ ERROR: Type 'undefined' is not assignable to type 'm.public1'.
    }
    function foo12() {
        return new public1();
    }

    export function foo13(): public1 {
        return;
        //~^ ERROR: Type 'undefined' is not assignable to type 'm.public1'.
    }
    export function foo14() {
        return new public1();
    }

    namespace m2 {
        export class public2 {
        }
    }

    function foo111(): m2.public2 {
        return;
        //~^ ERROR: Type 'undefined' is not assignable to type 'm2.public2'.
    }
    function foo112() {
        return new m2.public2();
    }

    export function foo113(): m2.public2 {
        return;
        //~^ ERROR: Type 'undefined' is not assignable to type 'm2.public2'.
    }
    export function foo114() {
        return new m2.public2();
    }
}
