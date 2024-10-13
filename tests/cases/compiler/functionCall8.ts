function foo(a?:string){}
foo('foo'); 
foo('foo', 'bar'); 
//~^ ERROR: Expected 0-1 arguments, but got 2.
foo(4);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
foo();
