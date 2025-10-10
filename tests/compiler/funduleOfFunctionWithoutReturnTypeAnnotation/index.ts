// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/funduleOfFunctionWithoutReturnTypeAnnotation.ts`, Apache-2.0 License

function fn() {
    return fn.n;
}
namespace fn {
    export var n = 1;
}
