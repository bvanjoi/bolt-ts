// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/classOrder2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A extends B { 
  //~^ ERROR: Class 'B' used before its declaration.

  foo() { this.bar(); }

}

class B {

  bar() { }

}


var a = new A();

a.foo();

