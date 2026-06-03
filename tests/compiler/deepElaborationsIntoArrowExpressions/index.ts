// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/deepElaborationsIntoArrowExpressions.ts`, Apache-2.0 License

//@compiler-options: target=es6

const a: {
    y(): "a"
} = {
    y: () => "b"
    //~^ ERROR: Type '"b"' is not assignable to type '"a"'.
};

interface Foo {
    a: number;
}

function foo1(): () => Foo {
    return () => ({a: ''});
    //~^ ERROR: Type '{ a: string; }' is not assignable to type 'Foo'.
}

function foo3(): Foo[] {
    return [{a: ''}];
    //~^ ERROR: Type 'string' is not assignable to type 'number'.
}
var y: Foo[] = [{a: ''}]
//~^ ERROR: Type 'string' is not assignable to type 'number'.