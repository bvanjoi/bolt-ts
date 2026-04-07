// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/contextualTypingOfLambdaWithMultipleSignatures.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

interface Foo {
    getFoo(n: number): void;
    getFoo(s: string): void;
}

var foo: Foo;
foo.getFoo = bar => { };