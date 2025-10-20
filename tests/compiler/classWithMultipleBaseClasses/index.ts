// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classWithMultipleBaseClasses.ts`, Apache-2.0 License

class A {
    foo() { }
}

class B {
    bar() { }
}

interface I {
    baz();
}

interface J {
    bat();
}


class D implements I, J {
  //~^ ERROR: Property 'foo' is missing.
  //~| ERROR: Property 'bar' is missing.
    baz() { }
    bat() { }
}

interface I extends A, B {
}