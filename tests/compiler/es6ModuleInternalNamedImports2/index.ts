// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es6ModuleInternalNamedImports2.ts`, Apache-2.0 License

//@compiler-options: target=ES6

export namespace M {
    // variable
    export var M_V = 0;
    // interface
    export interface M_I { }
    //calss
    export class M_C { }
    // instantiated module
    export namespace M_M { var x; }
    // uninstantiated module
    export namespace M_MU { }
    // function
    export function M_F() { }
    // enum
    export enum M_E { }
    // type
    export type M_T = number;
    // alias
    export import M_A = M_M;
}

export namespace M {
    // Reexports
    export {M_V as v};
    //~^ ERROR: Export declarations are not permitted in a namespace.
    export {M_I as i};
    //~^ ERROR: Export declarations are not permitted in a namespace.
    export {M_C as c};
    //~^ ERROR: Export declarations are not permitted in a namespace.
    export {M_M as m};
    //~^ ERROR: Export declarations are not permitted in a namespace.
    export {M_MU as mu};
    //~^ ERROR: Export declarations are not permitted in a namespace.
    export {M_F as f};
    //~^ ERROR: Export declarations are not permitted in a namespace.
    export {M_E as e};
    //~^ ERROR: Export declarations are not permitted in a namespace.
    export {M_A as a};
    //~^ ERROR: Export declarations are not permitted in a namespace.
}
