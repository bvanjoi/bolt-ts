// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/errorMessagesIntersectionTypes01.ts`, Apache-2.0 License

interface Foo {
    fooProp: boolean;
}

interface Bar {
    barProp: string;
}

interface FooBar extends Foo, Bar {
}

declare function mixBar<T>(obj: T): T & Bar;

let fooBar: FooBar = mixBar({ //~ERROR: Type '{ fooProp: string; } & Bar' is not assignable to type 'FooBar'.
    fooProp: "frizzlebizzle"
});