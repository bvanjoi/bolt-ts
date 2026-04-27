// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/indexSignatureOfTypeUnknownStillRequiresIndexSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function f<T extends unknown = unknown>(x: { [x: string]: T }): T;

var stooges = [
  { name: "moe", age: 40 },
  { name: "larry", age: 50 },
  { name: "curly", age: 60 }
];

f(stooges); // Should throw
//~^ ERROR: Argument of type '{ name: string; age: number; }[]' is not assignable to parameter of type '{ [x: string]: unknown }'.
