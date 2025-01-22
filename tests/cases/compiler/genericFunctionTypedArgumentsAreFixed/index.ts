// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericFunctionTypedArgumentsAreFixed.ts`, Apache-2.0 License

declare function map<T, U>(f: (x: T) => U, xs: T[]): U[];
map((a) => a.length, [1]);
//~^ ERROR: Property 'length' does not exist on type 'number'.