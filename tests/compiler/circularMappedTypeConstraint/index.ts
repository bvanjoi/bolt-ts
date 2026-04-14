// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/circularMappedTypeConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare function foo2<T extends { [P in keyof T & string as Capitalize<P>]: V }, V extends string>(a: T): T;
export const r2 = foo2({A: "a"});
