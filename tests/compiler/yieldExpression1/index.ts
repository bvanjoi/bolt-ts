// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/yieldStringLiteral.ts`, Apache-2.0 License

//@compiler-options: strictNullChecks
//@compiler-options: target=es6

function* a() {
    yield;
    yield 0;
}

function* b(): IterableIterator<number> {
    yield;
    //~^ ERROR: Type 'undefined' is not assignable to type 'number'.
    yield 0;
}
