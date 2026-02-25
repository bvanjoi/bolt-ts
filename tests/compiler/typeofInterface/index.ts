// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeofInterface.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var I: { a: string};

interface I {
    I: number;
    foo: typeof I;
}

var k: I;
var j: typeof k.foo = { a: "hello" };