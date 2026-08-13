// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/slightlyIndirectedDeepObjectLiteralElaborations.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo {
    a: {
        b: {
            c: {
                d: string
            }
        }
    }
}

let q: Foo["a"] | undefined;
const x: Foo = (void 0, {
    a: q = {
        b: ({
            c: {
                d: 42
                //~^ ERROR: Type 'number' is not assignable to type 'string'.
                //~| ERROR: Type 'number' is not assignable to type 'string'.
            }
        })
    }
});
