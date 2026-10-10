// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/yieldExpressionInnerCommentEmit.ts`, Apache-2.0 License

//@compiler-options: target=es6

function * foo2() {
    /*comment1*/ yield 1;
    yield /*comment2*/ 2;
    yield 3 /*comment3*/
    yield */*comment4*/ [4];
    yield /*comment5*/* [5];
}
