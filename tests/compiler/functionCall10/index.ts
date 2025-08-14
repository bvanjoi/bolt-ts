function foo(...a:number[]){};
foo(0, 1); 
foo('foo'); 
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
foo();
foo(1, 'bar');
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type 'number'.
