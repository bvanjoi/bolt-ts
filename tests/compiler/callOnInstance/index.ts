// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/callOnInstance.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function D(): string; // error

declare class D { constructor (value: number); } // error

var s1: string = D(); // OK

var s2: string = (new D(1))();
//~^ ERROR: This expression is not callable.

declare class C { constructor(value: number); }
(new C(1))(); // Error for calling an instance
//~^ ERROR: This expression is not callable.
