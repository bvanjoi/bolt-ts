// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/errorMessagesIntersectionTypes02.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo {
    fooProp: "hello" | "world";
}

interface Bar {
    barProp: string;
}

interface FooBar extends Foo, Bar {
}

declare function mixBar<T>(obj: T): T & Bar;

let fooBar: FooBar = mixBar({   //~ERROR: Type '{ fooProp: "frizzlebizzle"; } & Bar' is not assignable to type 'FooBar'.
    fooProp: "frizzlebizzle"
});