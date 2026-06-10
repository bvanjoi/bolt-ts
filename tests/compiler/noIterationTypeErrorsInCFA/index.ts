// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noIterationTypeErrorsInCFA.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict

interface F {
    d(): void
}
export function doRemove(dds: F | F[]) {
    if (!Array.isArray(dds)) {
        dds = [dds]
    }
    for (let n of dds) {
        n.d()
    }
    return dds
}
