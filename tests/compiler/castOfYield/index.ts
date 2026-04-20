// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/castExpressionParentheses.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function* f() {
    <number> (yield 0);
    // Unlike await, yield is not allowed to appear in a simple unary expression.
    <number> yield 0; //~ERROR: Expression expected.
}
