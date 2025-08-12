function foo(a:string, ...b:number[]){}
foo('foo', 1); 
foo('foo'); 
foo();
//~^ ERROR: Expected at least 1 arguments, but got 0.
foo(1, 'bar');
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
//~| ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
foo('foo', 1, 3);
