function foo(a:string, b?:string, ...c:number[]){}
foo('foo', 1); 
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
foo('foo'); 
foo('foo', 'bar'); 
foo();
//~^ ERROR: Expected at least 1 arguments, but got 0.
foo(1, 'bar');
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
foo('foo', 'bar', 3);
