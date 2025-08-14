function foo(a?:string, b?:number){};
foo('foo', 1); 
foo('foo'); 
foo('foo','bar');
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
foo('foo', 1, 'bar');
//~^ ERROR: Expected 0-2 arguments, but got 3.
foo();