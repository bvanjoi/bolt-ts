class A { }
class B { }
class C extends A, B { }
//~^ ERROR: Syntax Error: Unexpected token ','
//~| ERROR: Classes can only extend a single class.