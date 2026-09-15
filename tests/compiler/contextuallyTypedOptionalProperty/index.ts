// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextuallyTypedOptionalProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@[exactOptionalPropertyTypes=true]  compiler-options: exactOptionalPropertyTypes
//@[exactOptionalPropertyTypes=false] compiler-options: exactOptionalPropertyTypes=false
//@compiler-options: noEmit

declare function match<T>(cb: (value: T) => boolean): T;

declare function foo(pos: { x?: number; y?: number }): boolean;
foo({ y: match(y => y > 0) })
//~[exactOptionalPropertyTypes=false]^ ERROR: 'y' is possibly 'undefined'.
//~[exactOptionalPropertyTypes=false]| ERROR: 'y' is possibly 'undefined'.

declare function foo2(point: [number?]): boolean;
foo2([match(y => y > 0)])
//~[exactOptionalPropertyTypes=false]^ ERROR: 'y' is possibly 'undefined'.
//~[exactOptionalPropertyTypes=false]| ERROR: 'y' is possibly 'undefined'.
