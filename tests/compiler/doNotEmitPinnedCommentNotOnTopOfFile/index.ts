// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doNotEmitPinnedCommentNotOnTopOfFile.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: removeComments

var x = 10;

/*!

    multi line
    comment
*/

var x = 10;
