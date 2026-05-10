// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionTypeArgumentArityErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// Overloaded functions with default type arguments
declare function f1<A = any>(): void;
declare function f1<A, B, C, D = any>(): void;
f1<number, number>();
//~^ ERROR: No overload expects 2 type arguments, but overloads do exist that expect either 1 or 3 type arguments.
f1<number, number, number, number, number>();
//~^ ERROR: Expected 4 type arguments, but got 5.

// Overloaded functions with no default type arguments
declare function f2<A>(): void;
declare function f2<A, B, C>(): void;
f2<number, number>();
//~^ ERROR: No overload expects 2 type arguments, but overloads do exist that expect either 1 or 3 type arguments.
f2<number, number, number, number>();
//~^ ERROR: Expected 3 type arguments, but got 4.

// Overloaded non-generic functions
declare function f3(): void;
declare function f3(a): void;
f3<number>();
//~^ ERROR: Expected 0 type arguments, but got 1.

// Generic function with default type parameters
declare function f4<A, B, C = any>(): void;
f4<number>();
//~^ ERROR: Expected 2-3 type arguments, but got 1.
f4<number, number, number, number>();
//~^ ERROR: Expected 2-3 type arguments, but got 4.

// Generic function with no default type arguments
declare function f5<A, B>(): void;
f5<number>();
//~^ ERROR: Expected 2 type arguments, but got 1.
f5<number, number, number>();
//~^ ERROR: Expected 2 type arguments, but got 3.