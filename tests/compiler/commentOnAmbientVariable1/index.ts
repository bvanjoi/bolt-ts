// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentOnAmbientVariable1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

/*!=========
    Keep this pinned comment
   =========
*/

/*! Don't keep this pinned comment */
declare var v: number;

// Don't keep this comment.
declare var y: number;