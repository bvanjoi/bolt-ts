function foo(param:number[]){}; foo([1, "a"]);
//~^ ERROR: Argument of type 'number | string[]' is not assignable to parameter of type 'number[]'.