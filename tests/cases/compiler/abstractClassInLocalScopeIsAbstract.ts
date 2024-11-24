(() => {
    abstract class A {}
    class B extends A {}
    new A();
    //~^ ERROR: Cannot create an instance of an abstract class.
    new B();
})()
