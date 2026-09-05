// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursivelyExpandingUnionNoStackoverflow.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type N<T, K extends string> = T | { [P in K]: N<T, K> }[K];

type M = N<number, "M">;
//~^ ERROR: Type of property 'M' circularly references itself in mapped type '{ }'.