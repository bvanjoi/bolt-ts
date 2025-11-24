// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/narrowUnknownByTypeofObject.ts`, Apache-2.0 License

//@compiler-options: strictNullChecks

function foo(x: unknown) {
    if (typeof x === "object") {
        x
    }
}
