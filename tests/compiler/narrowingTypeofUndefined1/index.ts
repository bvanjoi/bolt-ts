// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/narrowingTypeofUndefined1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare const a: { error: { prop: string }, result: undefined } | { error: undefined, result: { prop: number } }

if (typeof a.error === 'undefined') {
    a.result.prop; // number
    let s1: boolean = a.result.prop;  //~ERROR: Type 'number' is not assignable to type 'boolean'.
}
else {
    a.error.prop; // string
    let s2: boolean = a.error.prop    //~ERROR: Type 'string' is not assignable to type 'boolean'.
}

if (typeof a.error !== 'undefined') {
    a.error.prop; // string
    let s2: boolean = a.error.prop    //~ERROR: Type 'string' is not assignable to type 'boolean'.  
}
else {
    a.result.prop; // number
    let s1: boolean = a.result.prop;  //~ERROR: Type 'number' is not assignable to type 'boolean'.
}
