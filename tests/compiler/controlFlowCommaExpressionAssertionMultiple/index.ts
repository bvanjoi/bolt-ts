// From `github.com/microsoft/TypeScript/blob/v5.8.3/tests/cases/compiler/controlFlowCommaExpressionAssertionMultiple.ts`, Apache-2.0 License

function Narrow<T>(value: any): asserts value is T {}

function func(foo: any, bar: any) {
    Narrow<number>(foo), Narrow<string>(bar);
    foo;
    bar;
    let b: number = bar;
    //~^ ERROR: Type 'string' is not assignable to type 'number'.
}

function func2(foo: any, bar: any, baz: any) {
    Narrow<number>(foo), Narrow<string>(bar), Narrow<boolean>(baz);
    foo;
    bar;
    baz;

    let a: number = foo;
}


{
    let a = 1;
    let b = ''
    let c0: number = (a, b);
    //~^ ERROR: Type 'string' is not assignable to type 'number'.
    let c1: number = (a, b);
    //~^ ERROR: Type 'string' is not assignable to type 'number'.
}