// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/awaitExpressionInnerCommentEmit.ts`, Apache-2.0 License

//@compiler-options: target=esnext

async function foo() {
    /*comment1*/ await 1;
    await /*comment2*/ 2;
    await 3 /*comment3*/
}
