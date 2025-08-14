// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/abstractClassInLocalScope.ts`, Apache-2.0 License

(() => {
    abstract class A {}
    class B extends A {}
    new B();
    return A;
})();
