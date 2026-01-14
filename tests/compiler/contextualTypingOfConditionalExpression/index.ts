// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTypingOfConditionalExpression.ts`, Apache-2.0 License

var x: (a: number) => void = true ? (a) => a.toExponential() : (b) => b.toFixed();

class A {
    foo: number;
}
class B extends A {
    bar: number;
}
class C extends A {
    baz: number;
}

var x2: (a: A) => void = true ? (a) => a.foo : (b) => b.foo;
