// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/overloadRet.ts`, Apache-2.0 License

interface I {
    f(s:string):number;  
    f(n:number):string;
    g(n:number):any; 
    g(n:number,m:number):string;
    h(n:number):I;  
    h(b:boolean):number;
    i(b:boolean):number;
    i(b:boolean):any;
}