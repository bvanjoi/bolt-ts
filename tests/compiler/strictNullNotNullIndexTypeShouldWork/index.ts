// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/strictNullNotNullIndexTypeShouldWork.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: strictPropertyInitialization=false

interface A {
    params?: { name: string; };
}

class Test<T extends A> {
    attrs: Readonly<T>;

    m() {
        this.attrs.params!.name;
    }
}

interface Foo {
    foo?: number;
}

class FooClass<P extends Foo = Foo> {
    properties: Readonly<P>;

    foo(): number {
        const { foo = 42 } = this.properties;
        return foo;
    }
}

class Test2<T extends A> {
    attrs: Readonly<T>;

    m() {
        return this.attrs.params!; // Return type should maintain relationship with `T` after being not-null-asserted, ideally
    }
}