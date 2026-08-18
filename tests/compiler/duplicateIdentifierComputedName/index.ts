// From `github.com/microsoft/TypeScript/blob/6.0.3/tests/cases/compiler/duplicateIdentifierDifferentModifiers.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C {
    ["a"]: string;
    //~^ ERROR: Property 'computed' has no initializer and is not definitely assigned in the constructor.
    ["a"]: string;
    //~^ ERROR: Duplicate identifier 'a'.
    //~| ERROR: Property 'computed' has no initializer and is not definitely assigned in the constructor.
}


class D {
    a: string;
    //~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
    a: string;
    //~^ ERROR: Duplicate identifier 'a'.
    //~| ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.
}

const fooBar: {} = {
	[Symbol(42)]: true,
};
