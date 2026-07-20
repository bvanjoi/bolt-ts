// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultValueInFunctionTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

type Foo = ({ first = 0 }: { first?: number }) => unknown;
//~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.

var x: (a: number = 1) => number;
//~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.
var y = <(a : string = "") => any>(undefined)
//~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.
//~| ERROR: Conversion of type 'undefined' to type '(a: string) => any' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.

type Bar = ({ first: second = 0 }: { first?: number }) => unknown;
//~^ ERROR: A parameter initializer is only allowed in a function or constructor implementation.
//~| ERROR: 'second' is an unused renaming of 'first'. Did you intend to use it as a type annotation?