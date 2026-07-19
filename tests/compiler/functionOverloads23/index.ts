// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionOverloads23.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false


function foo(bar:(b:string)=>void);
function foo(bar:(a:number)=>void);
function foo(bar:(a?)=>void) { return 0 }

foo((a) => {
  let b: string = a;
})

foo((a: number) => {
  let b: number = a;
})
