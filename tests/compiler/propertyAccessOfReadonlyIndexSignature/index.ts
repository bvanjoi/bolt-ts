// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyAccessOfReadonlyIndexSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Test {
    readonly [key: string]: string;
}

declare var a: Test;
a.foo = 'baz';
//~^ ERROR: Index signature in type 'Test' only permits reading.