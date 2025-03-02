// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/restParameters.ts`, Apache-2.0 License

function f18(a?:string, ...b:number[]){}
 
function f19(a?:string, b?:number, ...c:number[]){}
 
function f20(a:string, b?:string, ...c:number[]){}
 
function f21(a:string, b?:string, c?:number, ...d:number[]){}