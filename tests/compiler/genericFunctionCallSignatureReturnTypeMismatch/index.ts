// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericFunctionCallSignatureReturnTypeMismatch.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Array<T> {}

declare var f : { <T>(x:T): T; }

declare var g : { <S>() : S[]; };
f = g;
//~^ ERROR: Type '() => S[]' is not assignable to type '(x: T) => T'.

var s = f("str").toUpperCase();

console.log(s);
