// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/inferentialTypingWithObjectLiteralProperties.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f<T>(x: T, y: T): T {
return x;
}
f({ x: [null] }, { x: [1] }).x[0] = "" // ok
//~^ ERROR: Type 'string' is not assignable to type 'number'.
f({ x: [1] }, { x: [null] }).x[0] = "" // was error TS2011: Cannot convert 'string' to 'number'.
//~^ ERROR: Type 'string' is not assignable to type 'number'.
