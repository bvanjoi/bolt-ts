// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/yieldStringLiteral.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function yieldString() {
    yield 'literal';
    //~^ ERROR: A 'yield' expression is only allowed in a generator body.
}
