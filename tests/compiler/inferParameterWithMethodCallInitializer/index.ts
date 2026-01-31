// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inferParameterWithMethodCallInitializer.ts`, Apache-2.0 License

//@compiler-options: noImplicitAny

function getNumber(): number {
    return 1;
}
class Example {
    getNumber(): number {
        return 1;
    }
    doSomething(a = this.getNumber()): typeof a {
        return a;
    }
}
function weird(this: Example, a = this.getNumber()) {
    return a;
}
class Weird {
    doSomething(this: Example, a = this.getNumber()) {
        return a;
    }
}
