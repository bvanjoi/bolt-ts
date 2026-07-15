// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/detachedCommentAtStartOfFunctionBody2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class TestFile {
    foo(message: string): () => string {
        /// <summary>Test summary</summary>
        /// <param name="message" type="String" />
        /// <returns type="Function" />

        return () => message + this.name;
        //~^ ERROR: Property 'name' does not exist on type 'TestFile<TestFile>'.
    }
}