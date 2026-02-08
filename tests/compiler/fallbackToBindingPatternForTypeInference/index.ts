// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/externModuleClobber.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictFunctionTypes

declare function trans<T>(f: (x: T) => string): number;
trans(({a}) => a);
//~^ ERROR: Argument of type '({ a }: { a: any; }) => any' is not assignable to parameter of type '(x: unknown) => string'.
trans(([b,c]) => 'foo');
//~^ ERROR: Argument of type '([b, c]: [any?, any?]) => string' is not assignable to parameter of type '(x: unknown) => string'.
trans(({d: [e,f]}) => 'foo');
//~^ ERROR: Argument of type '({ d: [e, f] }: { d: [any?, any?]; }) => string' is not assignable to parameter of type '(x: unknown) => string'.
trans(([{g},{h}]) => 'foo');
//~^ ERROR: Argument of type '([{ g }, { h }]: [{ g: any; }?, { h: any; }?]) => string' is not assignable to parameter of type '(x: unknown) => string'.
trans(({a, b = 10}) => a);
//~^ ERROR: Argument of type '({ a, b }: { a: any; b: number; }) => any' is not assignable to parameter of type '(x: unknown) => string'.
