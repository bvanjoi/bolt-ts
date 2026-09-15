// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/truthinessCallExpressionCoercion3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: lib=[esnext,dom]

// from #41640, based on an example in ant-design
interface I {
    always(): void
}

function f(result: unknown) {
    if ((result as I).always) {
        return result
    }
}
function g(result: unknown) {
    if (((result as I)).always) {
        return result
    }
}

