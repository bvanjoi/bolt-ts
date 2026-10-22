// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsOnObjectLiteral5.ts`, Apache-2.0 License

//@compiler-options: removeComments=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

const a = {
    p0: 0, // Comment 0
    p1: 0, /* Comment 1
    A multiline comment. */
    p2: 0, // Comment 2
};
