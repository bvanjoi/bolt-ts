// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitFBoundedTypeParams.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

function append<a, b extends a>(result: a[], value: b): a[] {
    result.push(value);
    return result;
}