// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/contextuallyTypingOrOperator3.ts`, Apache-2.0 License

function foo<T, U extends T>(u: U) {
    var x3: U = u || u;
}
