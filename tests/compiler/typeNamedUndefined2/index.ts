// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeNamedUndefined2.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict
//@compiler-options: lib=[esnext]

export namespace ns {
    export namespace undefined {
        export const s = Symbol();
        export type undefined = typeof s;
        //~^ ERROR: Type alias name cannot be 'undefined'.
    };
    export function x(p: undefined): undefined {
        return p;
    }
}

export function x(p: ns.undefined.undefined) {
    return p;
}

export namespace undefined {
    export const s = Symbol();
    export type undefined = typeof s;
    //~^ ERROR: Type alias name cannot be 'undefined'.
};