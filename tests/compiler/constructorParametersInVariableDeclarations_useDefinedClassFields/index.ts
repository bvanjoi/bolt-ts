//@compiler-options: target=ES2022

class A {
    private a = x;
    //~^ ERROR: Cannot find name 'x'.
    private b = { p: x };
    //~^ ERROR: Cannot find name 'x'.
    private c = () => x;
    //~^ ERROR: Cannot find name 'x'.
    constructor(x: number) {
    }
}

class B {
    private a = x;
    //~^ ERROR: Cannot find name 'x'.
    private b = { p: x };
    //~^ ERROR: Cannot find name 'x'.
    private c = () => x;
    //~^ ERROR: Cannot find name 'x'.
    constructor() {
        var x = 1;
    }
}