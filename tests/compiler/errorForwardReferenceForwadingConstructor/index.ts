// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/errorForwardReferenceForwadingConstructor.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f() {
    var d1 = new derived(); //~ ERROR: Expected 1 arguments, but got 0.
    var d2 = new derived(4);
}

class base { constructor(public n: number) { } }
class derived extends base { }
