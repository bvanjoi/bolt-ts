class C { foo: number }
//~^ ERROR: Property 'foo' has no initializer and is not definitely assigned in the constructor.
class D extends C, { //~ ERROR: Syntax Error: Unexpected token ','
}
