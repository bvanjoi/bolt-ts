// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadsAndTypeArgumentArityErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare function Callbacks(flags?: string): void;
declare function Callbacks<T>(flags?: string): void;
declare function Callbacks<T1, T2>(flags?: string): void;

Callbacks<number, string, boolean>('s'); // wrong number of type arguments
//~^ ERROR: Expected 2 type arguments, but got 3.
new Callbacks<number, string, boolean>('s'); // wrong number of type arguments
//~^ ERROR: Expected 2 type arguments, but got 3.

declare function f<A, B = {}>(arg: number): void;
f<number>(); // wrong number of arguments (#25683)
//~^ ERROR: Expected 1 arguments, but got 0.
