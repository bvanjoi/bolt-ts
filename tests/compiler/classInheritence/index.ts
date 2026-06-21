class B extends A { }
//~^ ERROR: Class 'A' used before its declaration.
class A extends A { }
//~^ ERROR: 'A' is referenced directly or indirectly in its own base expression.