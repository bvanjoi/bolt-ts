// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/invokingNonGenericMethodWithTypeArguments2.ts`, Apache-2.0 License

class Foo {
    private foo: any;

    constructor() {
        this.foo<string>();
        //~^ ERROR: Untyped function calls may not accept type arguments.
    }
}
