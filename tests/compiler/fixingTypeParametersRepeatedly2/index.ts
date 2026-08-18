// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fixingTypeParametersRepeatedly2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Base {
    baseProp;
}
interface Derived extends Base {
    toBase(): Base;
}

declare var derived: Derived;

declare function foo<T>(x: T, func: (p: T) => T): T;
var result = foo(derived, d => d.toBase());
//~^ ERROR: Property 'toBase' is missing.

// bar should type check just like foo.
// The same error should be observed in both cases.
declare function bar<T>(x: T, func: (p: T) => T): T;
declare function bar<T>(x: T, func: (p: T) => T): T;
var result = bar(derived, d => d.toBase());
//~^ ERROR: Subsequent variable declarations must have the same type. Variable 'result' must be of type 'Derived', but here has type 'Base'.
