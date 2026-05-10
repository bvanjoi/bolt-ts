// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectBindingPatternContextuallyTypesArgument.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function id<T>(x: T): T;
const { f = (x: string) => x.length } = id({ f: x => x.charAt });
const g = f('');
const h: number = g;
//~^ ERROR: Type 'number | ((pos: number) => string)' is not assignable to type 'number'.

{
  const [ f = (x: string) => x.length ] = id([x => x.charAt]);
  const g = f('');
  const h: number = g;
  //~^ ERROR: Type 'number | ((pos: number) => string)' is not assignable to type 'number'.
}