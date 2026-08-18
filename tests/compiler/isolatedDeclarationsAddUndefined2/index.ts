// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/isolatedDeclarationsAddUndefined2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: isolatedDeclarations
//@compiler-options: declaration
//@compiler-options: strict

// https://github.com/microsoft/TypeScript/issues/60123

export class Bar {
    constructor(private x?: Array | undefined) {}
    //~^ ERROR: Generic type 'Array<T>' requires 1 type argument.
}

export class Bar2 {
    constructor(private x?: Array) {}
    //~^ ERROR: Generic type 'Array<T>' requires 1 type argument.
}

export class Bar3 {
    constructor(private x: Array | undefined) {}
    //~^ ERROR: Generic type 'Array<T>' requires 1 type argument.
}

export class Bar4 {
    constructor(private x: Array) {}
    //~^ ERROR: Generic type 'Array<T>' requires 1 type argument.
}

export function test1(x?: Array | undefined): void {}
//~^ ERROR: Generic type 'Array<T>' requires 1 type argument.

export function test2(x?: Unresolved | undefined): void {}
//~^ ERROR: Cannot find name 'Unresolved'.

export function test3(x?: Unresolved): void {}
//~^ ERROR: Cannot find name 'Unresolved'.
