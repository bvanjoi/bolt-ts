// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fixingTypeParametersRepeatedly3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

interface Base {
    baseProp;
}
interface Derived extends Base {
    toBase?(): Base;
}

var derived: Derived;

declare function foo<T>(x: T, func: (p: T) => T): T;
var result = foo(derived, d => d.toBase());

// bar should type check just like foo.
// result2 should have the same type as result
declare function bar<T>(x: T, func: (p: T) => T): T;
declare function bar<T>(x: T, func: (p: T) => T): T;
var result2 = bar(derived, d => d.toBase());