// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/superCallInsideObjectLiteralExpression.ts`, Apache-2.0 License

class A {
    foo() {
    }
}

class B extends A {
    constructor() {
        var x = {
            x: super()
        }
    }
}