// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/typePredicatesInUnion.ts`, Apache-2.0 License

//@compiler-options: target=es2015
interface A {
    pred(x: {}): x is boolean;
}
interface B {
    pred(x: {}): x is string;
}

type Or = A | B;

function f(o: Or, x: {}) {
    if (o.pred(x)) {
        x;
    }
}
