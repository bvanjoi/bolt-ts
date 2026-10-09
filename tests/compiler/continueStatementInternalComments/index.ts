// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/continueStatementInternalComments.ts`, Apache-2.0 License

//@compiler-options: target=es2015

foo: for (;;) {
    /*1*/ break /*2*/ foo /*3*/;
    /*1*/ continue /*2*/ foo /*3*/;
}