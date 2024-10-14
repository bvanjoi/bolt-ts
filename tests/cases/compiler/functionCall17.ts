function foo(a:string, b?:string, c?:number, ...d:number[]){}
foo('foo', 1); 
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
foo('foo'); 
foo();
//~^ ERROR: Expected at least 1 arguments, but got 0.
foo(1, 'bar');
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
foo('foo', 1, 3);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
foo('foo', 'bar', 3, 4);
