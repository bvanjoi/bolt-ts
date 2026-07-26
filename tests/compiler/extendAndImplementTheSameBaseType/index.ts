// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/extendAndImplementTheSameBaseType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    foo: number
    //~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
    bar() {}
}
class D extends C implements C {
    baz() { }
}

var c: C;
var d: D = new D();
d.bar();
d.baz();
d.foo;