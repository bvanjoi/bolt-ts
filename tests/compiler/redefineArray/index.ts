// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/redefineArray.ts`, Apache-2.0 License

//@compiler-options: target=es2015

Array = function (n:number, s:string) {return n;};
//~^ ERROR: Type '(n: number, s: string) => number' is missing the following properties from type 'ArrayConstructor': isArray, from, and 2 more.