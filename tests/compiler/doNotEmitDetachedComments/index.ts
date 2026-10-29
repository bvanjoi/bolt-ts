// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doNotEmitDetachedComments.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: removeComments

function foo1() {
    // Single line comment

    return 42;
}

function foo2() {
    /*

        multi line
        comment
    */

    return 42;
}

function foo3() {
    // Single line comment with more than one blank line


    return 42;
}

function foo4() {
    /*

        multi line comment with more than one blank line
    */

    return 42;
}
