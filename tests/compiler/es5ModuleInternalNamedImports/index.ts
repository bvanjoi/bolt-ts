// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/es5ModuleInternalNamedImports.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

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
    import * as M2 from "M2";
    //~^ ERROR: Import declarations in a namespace cannot reference a module.
    import M4 from "M4";
    //~^ ERROR: Import declarations in a namespace cannot reference a module.
    export import M5 = require("M5");
    //~^ ERROR: Import declarations in a namespace cannot reference a module.
}
import M3 from "M3";
//~^ ERROR: Cannot find module 'M3' or its corresponding type declarations.
