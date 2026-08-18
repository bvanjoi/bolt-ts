// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadsAndTypeArgumentArity.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

declare function Callbacks(flags?: string): void;
declare function Callbacks<T>(flags?: string): void;
declare function Callbacks<T1, T2>(flags?: string): void;
declare function Callbacks<T1, T2, T3>(flags?: string): void;

Callbacks<number, string, boolean>('s'); // no error
new Callbacks<number, string, boolean>('s'); // no error