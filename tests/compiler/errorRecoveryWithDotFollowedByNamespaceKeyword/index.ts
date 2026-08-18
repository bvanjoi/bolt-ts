// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorRecoveryWithDotFollowedByNamespaceKeyword.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@skip-message-match

namespace A {
    function foo() {
        if (true) {
            B.
            

        namespace B {
            export function baz() { }
}
