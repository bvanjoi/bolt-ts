// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralIndexerNoImplicitAny.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny

interface I {
    [s: string]: any;
}

var x: I = {
    p: null
}