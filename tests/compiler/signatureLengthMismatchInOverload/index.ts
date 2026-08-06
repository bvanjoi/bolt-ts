// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/signatureLengthMismatchInOverload.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f(callback: (arg: string, arg2: string) => void): void;
function f(callback: (arg: number) => void): void;
function f(callback: unknown) { }

f((arg: number, arg2: number) => {});
//~^ ERROR: No overload matches this call.
