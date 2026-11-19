// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportingContainingVisibleType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=amd

class Foo {
    public get foo() {
        var i: Foo;
        return i; // Should be fine (previous bug report visibility error).
        //~^ ERROR: Variable 'i' is used before being assigned.
 
    }
}
 
export var x = 5;
