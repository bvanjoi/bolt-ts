// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/invokingNonGenericMethodWithTypeArguments1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {
    constructor() {
        this.foo<string>();
        //~^ ERROR: Property 'foo' does not exist on type 'Foo<Foo>'.
    }
}