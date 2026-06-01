// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/signatureLengthMismatchWithOptionalParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function callee(n: number | undefined, m: string) { }

function caller(arg: (n?: number) => void) { }

caller(callee);
//~^ ERROR: Argument of type '(n: number, m: string) => void' is not assignable to parameter of type '(n: number) => void'.
