// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadResolutionOverCTLambda.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(b: (item: number) => boolean) { }
foo(a => a); // can not convert (number)=>bool to (number)=>number
//~^ ERROR: Type 'number' is not assignable to type 'boolean'.