// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/incorrectClassOverloadChain.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
    foo(): string;
    foo(x): number; //~ERROR: Function implementation is missing or not immediately following the declaration.
    x = 1;
}