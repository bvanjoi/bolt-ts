// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/detachedCommentAtStartOfConstructor2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class TestFile {
    public message: string;
    public name: string;
    //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
    constructor(message: string) {
        /// <summary>Test summary</summary>
        /// <param name="message" type="String" />

        var getMessage = () => message + this.name;
        this.message = getMessage();
    }
}
