// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyWrappedInTry.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@skip-message-match

class Foo {

    try {

        public bar = someInitThatMightFail();

    } catch(e) {}



    public baz() {

        return this.bar; // doesn't get rewritten to Foo.bar.

    }

}