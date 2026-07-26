// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadresolutionWithConstraintCheckingDeferred.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface A { x }
interface B { x; y }
interface C { z }
interface D { q }

class G<T extends A> {
    constructor(x: T) { }
}

declare function foo(arg: (x: D) => number): string;
declare function foo(arg: (x: C) => any): string;
declare function foo(arg: (x: B) => any): number;

var result: number = foo(x => new G(x)); // x has type D, new G(x) fails, so first overload is picked.
//~^ ERROR: No overload matches this call.
//~| ERROR: Property 'x' is missing.

var result2: number = foo(x => new G<typeof x>(x)); // x has type D, new G(x) fails, so first overload is picked.
//~^ ERROR: No overload matches this call.
//~| ERROR: Property 'x' is missing.

var result3: string = foo(x => { // x has type D
    //~^ ERROR: No overload matches this call.
    var y: G<typeof x> = new G(x); // error that D does not satisfy constraint, y is of type G<D>, entire call to foo is an error
    //~^ ERROR: Type 'D' does not satisfy the constraint 'A'.
    //~| ERROR: Property 'x' is missing.
    return y;
});