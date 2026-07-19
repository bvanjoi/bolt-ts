interface I6 {
  ():void;
}

interface I7 extends I6 { }

var v1:I7;
v1();
//~^ ERROR: Variable 'v1' is used before being assigned.
 
v1(42); 
//~^ ERROR: Expected 0 arguments, but got 1.
//~| ERROR: Variable 'v1' is used before being assigned.
v1('42');
//~^ ERROR: Expected 0 arguments, but got 1.
//~| ERROR: Variable 'v1' is used before being assigned.
