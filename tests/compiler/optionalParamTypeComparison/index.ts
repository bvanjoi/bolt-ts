// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalParamTypeComparison.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var f: (s: string, n?: number) => void;
declare var g: (s: string, b?: boolean) => void;

f = g;
//~^ ERROR: Type '(s: string, b: boolean) => void' is not assignable to type '(s: string, n: number) => void'.
g = f;
//~^ ERROR: Type '(s: string, n: number) => void' is not assignable to type '(s: string, b: boolean) => void'.