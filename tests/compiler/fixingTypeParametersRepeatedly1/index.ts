// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fixingTypeParametersRepeatedly1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

declare function f<T>(x: T, y: (p: T) => T, z: (p: T) => T): T;
f("", x => null, x => x.toLowerCase());

// First overload of g should type check just like f
declare function g<T>(x: T, y: (p: T) => T, z: (p: T) => T): T;
declare function g();
g("", x => null, x => x.toLowerCase());