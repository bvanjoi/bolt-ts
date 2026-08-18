// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportPrivateType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace foo {
    class C1 {
        x: string;
        //~^ ERROR: Property 'x' has no initializer and is not definitely assigned in the constructor.
        y: C1;
        //~^ ERROR: Property 'y' has no initializer and is not definitely assigned in the constructor.
    }
 
    class C2 {
        test() { return true; }
    }
 
    interface I1 {
        (a: string, b: string): string;
        (x: number, y: number): I1;
    }
 
    interface I2 {
        x: string;
        y: number;
    }
 
    // None of the types are exported, so per section 10.3, should all be errors
    export var e: C1;
    export var f: I1;
    export var g: C2;
    export var h: I2;
}
 
var y = foo.g; // Exported variable 'y' has or is using private type 'foo.C2'.

