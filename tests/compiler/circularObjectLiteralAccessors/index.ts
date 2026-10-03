// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularObjectLiteralAccessors.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

const a = {
    b: {
        get foo(): string {
            return a.foo;
        },
        set foo(value: string) {
            a.foo = value;
        }
    },
    foo: ''
};