// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/betterErrorForUnionCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015
declare const union: { a: string } | { b: string }
union("");
//~^ ERROR: This expression is not callable.

declare const fnUnion: { a: string } | ((a: string) => void)
fnUnion("");
//~^ ERROR: This expression is not callable.

declare const fnUnion2: (<T extends number>(a: T) => void) | (<T>(a: string) => void)
fnUnion2("");
//~^ ERROR: This expression is not callable.
