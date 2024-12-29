// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/typeofInterface.ts`, Apache-2.0 License

var I: { a: string};

interface I {
    I: number;
    foo: typeof I;
}

var k: I;
var j: typeof k.foo = { a: "hello" };