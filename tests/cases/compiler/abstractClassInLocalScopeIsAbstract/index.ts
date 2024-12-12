// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/abstractClassInLocalScopeIsAbstract.ts`, Apache-2.0 License

(() => {
    abstract class A {}
    class B extends A {}
    new A();
    //~^ ERROR: Cannot create an instance of an abstract class.
    new B();
})()
