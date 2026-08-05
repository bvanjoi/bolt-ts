// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyOrdering2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class Foo {
    constructor(public x, y) { }
       foo() {
        var a = this.x;
        return this.y;
        //~^ ERROR: Property 'y' does not exist on type 'Foo<Foo>'.
    }
}
