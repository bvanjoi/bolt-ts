// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeInferenceCacheInvalidation.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Callback<TFoo, TBar> = (foo: TFoo, bar: TBar) => any

declare function example<TFoo, TBar, TCallback extends Callback<TFoo, TBar>>(
    foo: TFoo,
    callback: TCallback,
    bar: TBar,
): TCallback

example(42, (foo, bar) => ({
    t: () => {
        let s: string = bar;
        let n: string = foo;
        //~^ ERROR: Type 'number' is not assignable to type 'string'.
    }
}), '42');

example(42, (foo, bar) => ({
    t() {
        let s: string = bar;
        let n: string = foo;
        //~^ ERROR: Type 'number' is not assignable to type 'string'.
    }
}), '42');
