// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/detachedCommentAtStartOfLambdaFunction2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class TestFile {
    name: string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
    foo(message: string): () => string {
        return (...x: string[]) =>
            /// <summary>Test summary</summary>
            /// <param name="message" type="String" />
            /// <returns type="Function" />

            message + this.name;
    }
}