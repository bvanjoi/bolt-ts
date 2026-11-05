import * as Lib from './file1';
//~^ ERROR: Import declaration conflicts with local declaration of 'Lib'.
namespace Lib { // should fail to merge
    export const foo: string = "";
}
Lib.foo; // should work
var x: Lib.Bar; // should be an error
//~^ ERROR: Namespace 'Lib' has no exported member 'Bar'.
export { Lib }