// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalArgsWithDefaultValues.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(x: number, y?:boolean=false, z?=0) {}
//~^ ERROR: Parameter cannot have question mark and initializer.
//~| ERROR: Parameter cannot have question mark and initializer.

class CCC {
    public foo(x: number, y?:boolean=false, z?=0) {}
//~^ ERROR: Parameter cannot have question mark and initializer.
//~| ERROR: Parameter cannot have question mark and initializer.
    static foo2(x: number, y?:boolean=false, z?=0) {}
//~^ ERROR: Parameter cannot have question mark and initializer.
//~| ERROR: Parameter cannot have question mark and initializer.
}

var a = (x?=0) => { return 1; };
//~^ ERROR: Parameter cannot have question mark and initializer.
var b = (x, y?:number = 2) => { x; };
//~^ ERROR: Parameter cannot have question mark and initializer.
