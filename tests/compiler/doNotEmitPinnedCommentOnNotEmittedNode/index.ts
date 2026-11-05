// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doNotEmitPinnedCommentOnNotEmittedNode.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@compiler-options: target=es2015
//@compiler-options: removeComments

class C {
    /*! remove pinned comment anywhere else */
    public foo(x: string, y: any)
    public foo(x: string, y: number) { }
}

var x = 10;

/*! remove pinned comment anywhere else */
declare var OData: any;
