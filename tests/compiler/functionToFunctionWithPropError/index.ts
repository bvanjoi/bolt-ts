// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionToFunctionWithPropError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare let x: { (): string; prop: number };
declare let y: { (): string; }

x = y;
//~^ ERROR: Property 'prop' is missing.
y = x;