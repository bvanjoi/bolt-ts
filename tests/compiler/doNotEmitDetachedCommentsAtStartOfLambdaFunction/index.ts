// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doNotEmitDetachedCommentsAtStartOfLambdaFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: removeComments

() => {
    // Single line comment

    return 0;
}

() => {
    /*
        multi-line comment
    */

    return 0;
}

() => {
    // Single line comment with more than one blank line


    return 0;
}

() => {
    /*
        multi-line comment with more than one blank line
    */


    return 0;
}
