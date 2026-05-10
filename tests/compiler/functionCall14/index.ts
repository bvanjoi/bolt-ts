function foo(a?:string, ...b:number[]){} 
foo('foo', 1); 
foo('foo'); 
foo();
foo(1, 'bar');
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
foo('foo', 1, 3);
