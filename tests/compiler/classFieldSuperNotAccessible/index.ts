// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classFieldSuperNotAccessible.ts`, Apache-2.0 License

//@compiler-options: target=esnext

class T {
    field = () => {}
    method() {}
}
class T2 extends T {
    f() {
        super.field() // error
        //~^ ERROR: 'field' defined by the parent class is not accessible in the child class via super.
        super.method()
    }
}

new T2().f()
