// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/funClodule.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare function foo();
declare namespace foo {
    export function x(): any;
}
declare class foo { } // Should error


declare class foo2 { }
declare namespace foo2 {
    export function x(): any;
}
declare function foo2(); // Should error


function foo3() { }
//~^ ERROR: Function with bodies can only merge with classes that are ambient.
namespace foo3 {
     export function x(): any { }
}
class foo3 { } // Should error
//~^ ERROR: Class declaration cannot implement overload list for 'foo3'.