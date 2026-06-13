// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/superPropertyElementNoUnusedLexicalThisCapture.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A { x() {} }

class B extends A {
    constructor() {
        super();
    }
    foo() {
        return () => {
            super.x;
        }
    }
    bar() {
        return () => {
            super["x"];
        }
    }
}
