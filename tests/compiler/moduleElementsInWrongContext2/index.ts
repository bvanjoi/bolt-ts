// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleElementsInWrongContext2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function blah () {
    namespace M { }
    //~^ ERROR: A namespace declaration is only allowed at the top level of a namespace or module.
    export namespace N {
    //~^ ERROR: A namespace declaration is only allowed at the top level of a namespace or module.
    //~| ERROR: An export declaration can only be used at the top level of a namespace or module.
        export interface I { }
    }

    namespace Q.K { }
    //~^ ERROR: A namespace declaration is only allowed at the top level of a namespace or module.

    declare module "ambient" {
    //~^ ERROR: An ambient module declaration is only allowed at the top level in a file.
    //~| ERROR: Ambient modules cannot be nested in other modules or namespaces.
    }

    export = M;
    //~^ ERROR: An export assignment must be at the top level of a file or module declaration.
    //~| ERROR: Export assignment cannot be used when targeting ECMAScript modules. Consider using 'export default' or another module format instead.

    var v;
    function foo() { }
    export * from "ambient";
    //~^ ERROR: An export declaration can only be used at the top level of a namespace or module.
    //~| ERROR: Export declarations are not permitted in a namespace.
    export { foo };
    //~^ ERROR: An export declaration can only be used at the top level of a namespace or module.
    //~| ERROR: Export declaration conflicts with exported declaration of 'foo'.
    export { baz as b } from "ambient";
    //~^ ERROR: An export declaration can only be used at the top level of a namespace or module.
    //~| ERROR: Export declarations are not permitted in a namespace.
    export default v;
    //~^ ERROR: A default export must be at the top level of a file or module declaration.
    export default class C { }
    //~^ ERROR: A default export must be at the top level of a file or module declaration.
    export function bee() { }
    //~^ ERROR: An export declaration can only be used at the top level of a namespace or module.
    import I = M;
    //~^ ERROR: An import declaration can only be used at the top level of a namespace or module.
    import I2 = require("foo");
    //~^ ERROR: An import declaration can only be used at the top level of a namespace or module.
    //~| ERROR: Import declarations in a namespace cannot reference a module.
    import * as Foo from "ambient";
    //~^ ERROR: An import declaration can only be used at the top level of a namespace or module.
    import bar from "ambient";
    //~^ ERROR: An import declaration can only be used at the top level of a namespace or module.
    import { baz } from "ambient";
    //~^ ERROR: An import declaration can only be used at the top level of a namespace or module.
    import "ambient";
    //~^ ERROR: An import declaration can only be used at the top level of a namespace or module.
}