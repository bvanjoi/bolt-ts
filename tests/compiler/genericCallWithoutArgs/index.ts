// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericCallWithoutArgs.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f<X, Y>(x: X, y: Y) {
}

f<number,string>. //~ERROR: Identifier expected.