// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionOverloads21.ts`, Apache-2.0 License

function foo(bar:{a:number;}[]);
//~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
function foo(bar:{a:number; b:string;}[]);
//~^ ERROR: 'foo', which lacks return-type annotation, implicitly has an 'any' return type.
function foo(bar:{a:any; b?:string;}[]) { return 0 }
