// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionParameterArityMismatch.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare function f1(a: number);
declare function f1(a: number, b: number, c: number);
f1();
//~^ ERROR: Expected 1-3 arguments, but got 0.
f1(1, 2);
//~^ ERROR: No overload expects 2 arguments, but overloads do exist that expect either 1 or 3 arguments.
f1(1, 2, 3, 4);
//~^ ERROR: Expected 1-3 arguments, but got 4.

declare function f2();
declare function f2(a: number, b: number);
declare function f2(a: number, b: number, c: number, d: number);
declare function f2(a: number, b: number, c: number, d: number, e: number, f: number);
f2(1);
//~^ ERROR: No overload expects 1 arguments, but overloads do exist that expect either 0 or 2 arguments.
f2(1, 2, 3);
//~^ ERROR: No overload expects 3 arguments, but overloads do exist that expect either 2 or 4 arguments.
f2(1, 2, 3, 4, 5);
//~^ ERROR: No overload expects 5 arguments, but overloads do exist that expect either 4 or 6 arguments.
f2(1, 2, 3, 4, 5, 6, 7);
//~^ ERROR: Expected 0-6 arguments, but got 7.
f2(1, 2, 3, 4, 5, ...[6, 7]);
//~^ ERROR: Expected 0-6 arguments, but got 7.
