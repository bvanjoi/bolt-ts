// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/signatureLengthMismatchCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function takesCallback(fn: (a: number) => void) {
  // ...
}

takesCallback((a: number, b: number) => {});
//~^ ERROR: Argument of type '(a: number, b: number) => void' is not assignable to parameter of type '(a: number) => void'.
