// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/functionOverloads23.ts`, Apache-2.0 License

function foo(bar:(b:string)=>void);
function foo(bar:(a:number)=>void);
function foo(bar:(a?)=>void) { return 0 }

foo((a) => {
  let b: string = a;
})

foo((a: number) => {
  let b: number = a;
})
