// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadsWithConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

declare function f<T extends Number>(x: T): T;
declare function f<T extends String>(x: T): T

var v = f<string>("");