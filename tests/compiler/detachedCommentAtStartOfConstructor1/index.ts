// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/detachedCommentAtStartOfConstructor1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class TestFile {
    public message: string;
    public name;
    constructor(message: string) {
        /// <summary>Test summary</summary>
        /// <param name="message" type="String" />
        var getMessage = () => message + this.name;
        this.message = getMessage();
    }
}