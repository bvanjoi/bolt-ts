export default abstract class A { a: number; }
//~^ ERROR: Property 'a' has no initializer and is not definitely assigned in the constructor.

class B extends A {}
new B().a.toExponential();