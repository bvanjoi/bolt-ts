// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/methodSignatureHandledDeclarationKindForSymbol.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Foo {
    bold(): string;
}

interface Foo {
    bold: string;
    //~^ ERROR: Subsequent property declarations must have the same type. Property 'bold' must be of type '() => string', but here has type 'string'.
}