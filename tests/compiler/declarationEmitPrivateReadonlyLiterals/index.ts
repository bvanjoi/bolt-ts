// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitPrivateReadonlyLiterals.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class Foo {
    private static readonly A = "a";
    private readonly B = "b";
    private static readonly C = 42;
    private readonly D = 42;
}