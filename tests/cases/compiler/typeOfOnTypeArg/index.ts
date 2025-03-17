var A = { '': 3 };

function fill<B extends typeof A>(f: B) {

} 

fill(32);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type '{ '': number; }'.
