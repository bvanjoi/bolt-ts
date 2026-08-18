// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/setterBeforeGetter.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {

    private _bar: { a: string; };
    //~^ ERROR: Property '_bar' has no initializer and is not definitely assigned in the constructor.
    // should not be an error to order them this way
    set bar(thing: { a: string; }) {
        this._bar = thing;
    }
    get bar(): { a: string; } {
        return this._bar;
    }
}
