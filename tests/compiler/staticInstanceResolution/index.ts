// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticInstanceResolution.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

class Comment {

    public getDocCommentText()
    {

    }

    static getDocCommentText(comments: Comment[])
    {
        comments[0].getDocCommentText();
        var c: Comment;
        c.getDocCommentText();
        //~^ ERROR: Variable 'c' is used before being assigned.
    }
}
