// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingOrderIndependent.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

class A {
    constructor(public stringOrUndefined: string | undefined) {}
}

class B {
    constructor(public str: string) {}
}

const a = new A("123");

if (a instanceof A && a.stringOrUndefined) {
    new B(a.stringOrUndefined)
}

if (a.stringOrUndefined && a instanceof A) {
    new B(a.stringOrUndefined)
}

if (a instanceof A) {
    if (a.stringOrUndefined) {
        new B(a.stringOrUndefined)
    }
}

if (a.stringOrUndefined) {
    if (a instanceof A) {
        new B(a.stringOrUndefined)
    }
}
