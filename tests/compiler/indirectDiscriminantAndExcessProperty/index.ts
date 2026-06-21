// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indirectDiscriminantAndExcessProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015

export type Blah =
    | { type: "foo", abc: string }
    | { type: "bar", xyz: number, extra: any };

declare function thing(blah: Blah): void;

let foo1 = "foo";
thing({
    type: foo1,
    //~^ ERROR: Type 'string' is not assignable to type '"foo" | "bar"'.
    abc: "hello!"
});

let foo2 = "foo";
thing({
    type: foo2,
    //~^ ERROR: Type 'string' is not assignable to type '"foo" | "bar"'.
    abc: "hello!",
    extra: 123,
});

let bar = "bar";
thing({
    type: bar,
    //~^ ERROR: Type 'string' is not assignable to type '"foo" | "bar"'.
    xyz: 123,
    extra: 123,
});
