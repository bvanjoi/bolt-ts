// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/asyncFunctionReturnExpressionErrorSpans.ts`, Apache-2.0 License

// @compiler-options: target=es2015

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

declare function f<T, U extends unknown[]>(
    callback: (...args0: U) => T , ...args1: U
): void;
f(() => "Hello");
f((foo) => "Hello", 'foo');
f((foo: string) => "Hello");
//~^ ERROR: Expected 2 arguments, but got 1.
f((foo) => "Hello");
//~^ ERROR: Expected 2 arguments, but got 1.
