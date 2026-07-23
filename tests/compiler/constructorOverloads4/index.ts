// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/constructorOverloads4.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare namespace M {    
    export class Function {
        constructor(...args: string[]);
    }
    export function Function(...args: any[]): any;
    export function Function(...args: string[]): Function;
}


(new M.Function("return 5"))(); //~ ERROR: This expression is not callable.
M.Function("yo");
