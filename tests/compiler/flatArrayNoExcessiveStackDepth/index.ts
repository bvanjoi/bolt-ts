// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/flatArrayNoExcessiveStackDepth.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: declaration
//@compiler-options: target=esnext

// Repro from #43493

declare const foo: unknown[];
const bar = foo.flatMap(bar => bar as Foo);

interface Foo extends Array<string> {}

// Repros from comments in #43249

const repro_43249 = (value: unknown) => {
    if (typeof value !== "string") {
        throw new Error("No");
    }
    const match = value.match(/anything/) || [];
    const [, extracted] = match;
};

function f<Arr, D extends number>(x: FlatArray<Arr, any>, y: FlatArray<Arr, D>) {
    x = y;
    y = x;  // Error
    //~^ ERROR: Type 'Arr | cond' is not assignable to type '{ done: Arr; recur: cond; }[cond]'.
}
