// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doNotEmitDetachedCommentsAtStartOfConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: removeComments

class A {
    constructor() {
        // Single Line Comment

        var x = 10;
    }
}

class B {
    constructor() {
        /* 
            Multi-line comment
        */

        var y = 10;
    }
}

class C {
    constructor() {
        // Single Line Comment with more than one blank line


        var x = 10;
    }
}

class D {
    constructor() {
        /* 
            Multi-line comment with more than one blank line
        */


        var y = 10;
    }
}
