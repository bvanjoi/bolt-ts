function foo(a:string){}; 
foo('bar');
foo(2);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
foo(1, 'bar');
//~^ ERROR: Expected 1 arguments, but got 2.
foo();
//~^ ERROR: Expected 1 arguments, but got 0.
