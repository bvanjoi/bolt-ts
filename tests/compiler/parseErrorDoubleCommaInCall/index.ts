// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseErrorDoubleCommaInCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015

Boolean({
    x: 0,,  //~ERROR: Property assignment expected.
});
