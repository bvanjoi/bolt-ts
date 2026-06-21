// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeNamedUndefined1.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=esnext
//@compiler-options: lib=[esnext]

export namespace ns {
    const s = Symbol();
    export type undefined = typeof s;
    //~^ ERROR: Type alias name cannot be 'undefined'.
    export function x(p: undefined): undefined { // global undefined
        return p;
    }
}

export function x(p: ns.undefined) { // undefined from the namespace
    return p;
}

export type undefined = "";
//~^ ERROR: Type alias name cannot be 'undefined'.
