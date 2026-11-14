// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/fallbackToBindingPatternForTypeInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
declare function trans<T>(f: (x: T) => string): number;
trans(({a}) => a);
//~^ ERROR: Argument of type '({ a }: { a: any; }) => any' is not assignable to parameter of type '(x: unknown) => string'.
trans(([b,c]) => 'foo');
//~^ ERROR: Argument of type '([b, c]: [any?, any?]) => string' is not assignable to parameter of type '(x: unknown) => string'.
trans(({d: [e,f]}) => 'foo');
//~^ ERROR: Property '"d"' does not exist on type 'unknown'.
//~| ERROR: Property '"d"' does not exist on type 'unknown'.
//~| ERROR: Argument of type '({ d: [e, f] }: { d: [any?, any?]; }) => string' is not assignable to parameter of type '(x: unknown) => string'.
trans(([{g},{h}]) => 'foo');
//~^ ERROR: Property '"g"' does not exist on type 'undefined | { g: any; }'.
//~| ERROR: Property '"h"' does not exist on type 'undefined | { h: any; }'.
//~| ERROR: Argument of type '([{ g }, { h }]: [undefined | { g: any; }?, undefined | { h: any; }?]) => string' is not assignable to parameter of type '(x: unknown) => string'.
//~| ERROR: Type 'T' must have a '[Symbol.iterator]()' method that returns an iterator.
//~| ERROR: Type 'T' must have a '[Symbol.iterator]()' method that returns an iterator.
trans(({a, b = 10}) => a);
//~^ ERROR: Argument of type '({ a, b }: { a: any; b: undefined | number; }) => any' is not assignable to parameter of type '(x: unknown) => string'.
