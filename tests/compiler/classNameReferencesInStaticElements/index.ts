// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classNameReferencesInStaticElements.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// https://github.com/microsoft/TypeScript/issues/54607
class Foo {
    static { console.log(this, Foo) }
    static x = () => { console.log(this, Foo) }
    static y = function(this: unknown) { console.log(this, Foo) }

    #x() { console.log(Foo); }
    x() { this.#x(); }
}

const oldFoo = Foo;
(Foo as any) = null;
oldFoo.x();
oldFoo.y();
new oldFoo().x();