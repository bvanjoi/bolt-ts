// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionExportsRequireAndUninstantiatedModule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=amd

export namespace require { // no error 
    export interface I {
    }
}
export function foo(): require.I {
    return null;
    //~^ ERROR: Type 'null' is not assignable to type 'require.I'.
}
export namespace exports { // no error
    export interface I {
    }
}
export function foo2(): exports.I {
    return null;
    //~^ ERROR: Type 'null' is not assignable to type 'exports.I'.
}
