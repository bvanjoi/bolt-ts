// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/detachedCommentAtStartOfConstructor1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class TestFile {
  message;
  name;
  constructor(message) {var getMessage = () => (message + this.name);
    this.message = getMessage();}
}