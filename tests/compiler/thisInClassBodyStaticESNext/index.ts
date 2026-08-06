// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/thisInClassBodyStaticESNext.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: useDefineForClassFields

// all are allowed with es-compliant class field emit
class Foo {
    x = this
    static t = this
    static at = () => this
    static ft = function () { return this }
    //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
    static mt() { return this }
}