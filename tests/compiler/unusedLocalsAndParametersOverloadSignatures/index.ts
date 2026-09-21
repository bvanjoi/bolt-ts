// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unusedLocalsAndParametersOverloadSignatures.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noUnusedLocals
//@compiler-options: noUnusedParameters

export function func(details: number, message: string, ...args: any[]): void;
export function func(details: number, message: string): any {
    return details + message;
}

export class C {
    constructor(details: number, message: string, ...args: any[]);
    constructor(details: number, message: string) {
        details + message;
    }

    method(details: number, message: string, ...args: any[]): void;
    method(details: number, message: string): any {
        return details + message;
    }
}


export function genericFunc<T>(details: number, message: T, ...args: any[]): void;
export function genericFunc(details: number, message: any): any {
    return details + message;
}