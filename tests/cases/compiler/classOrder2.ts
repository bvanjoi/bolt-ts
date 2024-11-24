
class A extends B { //~ERROR: Class 'B' used before its declaration.

  foo() { this.bar(); }

}

class B {

  bar() { }

}


var a = new A();

a.foo();

