// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constructorArgsErrors3.ts`, Apache-2.0 License

class foo {
    constructor (public public a: number) { //~ ERROR: Accessibility modifier already seen.
    }
}
