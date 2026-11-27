// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/isolatedModulesImportExportElision.ts`, Apache-2.0 License

//@compiler-options: isolatedModules
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=commonjs

import {c} from "module"
//~^ ERROR: Cannot find module 'module' or its corresponding type declarations.
import {c2} from "module"
//~^ ERROR: Cannot find module 'module' or its corresponding type declarations.
import * as ns from "module"
//~^ ERROR: Cannot find module 'module' or its corresponding type declarations.

class C extends c2.C {
}

let x = new c();
let y = ns.value;

export {c1} from "module";
//~^ ERROR: Cannot find module 'module' or its corresponding type declarations.
export var z = x;
