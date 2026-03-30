// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/generatorReturnExpressionIsChecked.ts`, Apache-2.0 License

//@compiler-options: lib=esnext
//@compiler-options: target=esnext

function* f(): Iterator<number> {
    return invalid;
    //~^ ERROR: Cannot find name 'invalid'.
}
