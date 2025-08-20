// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTypingWithGenericSignature.ts`, Apache-2.0 License

var f2: {
  <T, U>(x: T, y: U): T
};

f2 = (x, y) => { return x }
f2 = (x, y) => { return y }
//~^ ERROR: Type '(x: T, y: U) => U' is not assignable to type '(x: T, y: U) => T'.