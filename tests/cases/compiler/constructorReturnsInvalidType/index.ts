class X {
    constructor() {
        return 1;
        //~^ ERROR: Type 'number' is not assignable to type 'X'.
    }
    foo() { }
}
 
var x = new X();
