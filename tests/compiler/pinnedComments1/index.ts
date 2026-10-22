// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/pinnedComments1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: removeComments

/*!=========
    Keep this pinned comment
   =========
*/

/* unpinned comment */
/*! pinned comment that need to be removed */
class C {
}