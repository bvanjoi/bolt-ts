// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeofInternalModules.ts`, Apache-2.0 License

//@compiler-options: target=es2015
namespace Outer {
    export namespace instantiated {
        export class C { }
    }
    export namespace uninstantiated {
        export interface P { }
    }
}

import importInst = Outer.instantiated;
import importUninst = Outer.uninstantiated;

var x1: typeof importInst.C = importInst.C;
var x2: importInst.C = new x1();
var x3: typeof importUninst.P; // Error again
//~^ ERROR: Cannot find name 'importUninst'.

var x4: Outer = Outer;
//~^ ERROR: Cannot find name 'Outer'.
var x5: typeof importInst;
x5 = Outer;
//~^ ERROR: Property 'C' is missing.
x5 = Outer.instantiated;
var x6: typeof importUninst;
//~^ ERROR: Cannot find name 'importUninst'.
var x7: typeof Outer = Outer;
x7 = importInst;
//~^ ERROR: Property 'instantiated' is missing.
