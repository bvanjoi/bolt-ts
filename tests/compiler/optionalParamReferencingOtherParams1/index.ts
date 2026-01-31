// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/optionalParamReferencingOtherParams1.ts`, Apache-2.0 License

function strange(x: number, y = x * 1, z = x + y) {
    return z;
}