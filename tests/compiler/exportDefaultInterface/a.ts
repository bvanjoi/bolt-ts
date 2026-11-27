export default interface A { value: number; }

var a: A;
a.value.toExponential();
//~^ ERROR: Variable 'a' is used before being assigned.