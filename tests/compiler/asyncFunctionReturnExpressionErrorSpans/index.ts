// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/asyncFunctionReturnExpressionErrorSpans.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo {
    bar: {
        baz: {
            inner: {
                thing: string
            }
        }
    }
}

async function asyncFoo(): Promise<Foo> {
    return {
        bar: {
            baz: {
                inner: {
                    thing: 1
                    //~^ ERROR: Type 'number' is not assignable to type 'string'.
                }
            }
        }
    }
}